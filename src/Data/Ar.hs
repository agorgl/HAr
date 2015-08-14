-----------------------------------------------------------------------------
-- |
-- Module      : Data.Ar
-- Copyright   : (c) Agorgianitis Loukas, 2015
-- License     : MIT
--
-- Maintainer  : Agorgianitis Loukas <agorglouk@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- A module for parsing System V / GNU style ar files
--
-----------------------------------------------------------------------------
module Data.Ar (
    parseAr,
    Archive(..),
    ArchiveEntry(..)
) where

import Data.Char
import Data.Int (Int64)
import Data.Binary
import Data.Binary.Get
import Data.List
import Data.List.Split
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
-- import Debug.Trace
-- debug :: a -> String -> a
-- debug = flip trace

-- Strips the trailing whitespace
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

---------------------------------------------------------------------------
-- Ar Parser
---------------------------------------------------------------------------
-- The data type that will hold the final information about the archive
data Archive = Archive
    { symbols :: [String]
    , entries :: [ArchiveEntry]
    } deriving (Eq, Show)

-- A single file stored in the archive
data ArchiveEntry = ArchiveEntry
    { fileName :: String        -- ^ The filename of the entry
    , fileSize :: Int64         -- ^ The size of the file in bytes
    , fileData :: L.ByteString  -- ^ The file data
    } deriving (Eq)

-- Used for pretty printing
instance Show ArchiveEntry where
    show a = "Entry: " ++ show (fileName a) ++ ", size: " ++ show (fileSize a)

-- Changes the fileName of the given ArchiveEntry
-- Used when processing the extended filenames
changeFileName :: String -> ArchiveEntry -> ArchiveEntry
changeFileName f ar = ArchiveEntry { fileName=f, fileSize=fileSize ar, fileData=fileData ar }

---------------------------------------------------------------------------
-- Parsing
---------------------------------------------------------------------------
-- Parses and validates the header of the archive file
parseHeader :: Get ()
parseHeader = do
    magic <- getLazyByteString 8
    when (magic /= C.pack "!<arch>\n") $
        fail "Unrecognized header"

--
-- File entry header
-------------------------------------------------
-- Offset | Length | Name                        |
-------------------------------------------------
-- 0      | 16     | File name                   |
-- 16     | 12     | File modification timestamp |
-- 28     | 6      | Owner ID                    |
-- 34     | 6      | Group ID                    |
-- 40     | 8      | File mode                   |
-- 48     | 10     | File size in bytes          |
-- 58     | 2      | File magic (0x60 0x0A)      |
-------------------------------------------------
--
-- Parses each archive entry header
-- and returns the name and size of the stored file
parseEntryHeader :: Get (String, Int64)
parseEntryHeader = do
    fName <- rstrip . C.unpack <$> getLazyByteString 16
    skip 32
    fSize <- read . C.unpack <$> getLazyByteString 10 :: Get Int64
    skip 2
    return (fName, fSize)

--
-- The members are aligned to even byte boundaries.
-- Each archive file member begins on an even byte boundary;
-- a newline is inserted between files if necessary.
-- Nevertheless, the size given reflects the actual size of the file exclusive of padding.
--

-- Parses and returns all the files stored in the archive
parseEntries :: Get [ArchiveEntry]
parseEntries = do
    e <- isEmpty
    if e then
        return []
    else do
        (fName, fSize) <- parseEntryHeader
        fData <- getLazyByteString fSize
        offset <- bytesRead
        when (odd offset) $ skip 1
        rest <- parseEntries
        return $ ArchiveEntry fName fSize fData : rest

---------------------------------------------------------------------------
-- Processing
---------------------------------------------------------------------------
--
-- System V ar uses a '/' character (0x2F) to mark the end of the filename;
-- this allows for the use of spaces without the use of an extended filename.
-- Then it stores multiple extended filenames in the data section of a file 
-- with the name "//", this record is referred to by future headers.
-- A header references an extended filename by storing a "/" followed by a
-- decimal offset to the start of the filename in the extended filename data section.
-- The format of this "//" file itself is simply a list of the long filenames,
-- each separated by one or more LF characters.
-- Note that the decimal offsets are number of characters,
-- not line or string number within the "//" file.
--

-- Processes the data of the name table file "//"
-- and returns an assosiation list with offsets and extended filenames
getNameTable :: L.ByteString -> [(Int64, String)]
getNameTable = 
    let getName curOffset bs = 
            if L.length bs == 0
                then []
                else
                    let name = C.unpack $ L.takeWhile (/=0x0a) bs
                        newLines = C.unpack $ (L.takeWhile (==0x0a) . L.drop (fromIntegral (length name))) bs
                        sizeParsed = (fromIntegral $ length name + length newLines) :: Int64
                        newOffset = curOffset + sizeParsed
                        rest = L.drop sizeParsed bs
                    in (curOffset, name) : getName newOffset rest
    in getName 0

--
-- System V ar uses the special filename "/" to denote that the following data entry
-- contains a symbol lookup table, which is used in ar libraries to speed up access.
-- This symbol table is built in three parts which are recorded together as contiguous data.
--  1. A 32-bit big endian integer, giving the number of entries in the table.
--  2. A set of 32-bit big endian integers. One for each symbol,
--     recording the position within the archive of the header for the file containing this symbol.
--  3. A set of Zero-terminated strings. Each is a symbol name, and occurs in the same order
--     as the list of positions in part 2.
--
-- Processes the data of the symbol table file "/"
-- and returns a list with the symbol names
getSymbolList :: L.ByteString -> [String]
getSymbolList s = filter (/="") (splitOn "\0" (runGet
    (do symbolCount <- getWord32be
        _ <- getLazyByteString $ fromIntegral symbolCount * 4
        C.unpack <$> getRemainingLazyByteString) s))
 
-- Expands the extended filenames in an archive
replaceExtendedFileNames :: [ArchiveEntry] -> [ArchiveEntry]
replaceExtendedFileNames a =
    case find ((=="//") . fileName) a of
        Nothing -> a
        Just f  ->
            let nameTable = getNameTable $ fileData f
            in map (\ae -> let fname = fileName ae
                           in if "/" `isPrefixOf` fname && fname `notElem` ["/", "//"]
                                  then
                                      let offset = (read (tail $ fileName ae) :: Int64)
                                      in case lookup offset nameTable of
                                             Nothing  -> ae
                                             Just efn -> changeFileName efn ae
                                  else ae) a

-- Converts the contents of an archive file into the corresponding data type
parseAr :: L.ByteString -> Either String Archive
parseAr s = case runGetOrFail parseHeader s of
                Left _ -> Left "Could not parse header"
                Right (bs, _, _) ->
                    case runGetOrFail parseEntries bs of
                        Left _ -> Left "Could not parse entries"
                        Right (_,_,ae) -> Right $
                            let symbolTable = 
                                 case find ((=="/") . fileName) ae of
                                     Nothing -> []
                                     Just f  -> getSymbolList $ fileData f
                            in Archive symbolTable (replaceExtendedFileNames ae)

