-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) Agorgianitis Loukas, 2015
-- License     : MIT
--
-- Maintainer  : Agorgianitis Loukas <agorglouk@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Main of har executable
--
-----------------------------------------------------------------------------
import Control.Monad
import Control.Exception
import Data.List
import Data.Ar
import Options.Applicative
import System.FilePath
import System.Directory
import Text.Regex.PCRE
import Text.Regex.PCRE.String
import qualified Data.ByteString.Lazy as L

---------------------------------------------------------------------------
-- Command Line Options
---------------------------------------------------------------------------
-- Helper
withInfo :: Parser a -> String -> ParserInfo a
opts `withInfo` desc = info (helper <*> opts) $ progDesc desc

-- The Argument Data
data Command = SymbolList FilePath | FileList FilePath | SymbolSearch String FilePath
data Options = Options { cmd :: Command }

-- SubParsers
parseOptionSymbolList :: Parser Command
parseOptionSymbolList = SymbolList
    <$> argument str (metavar "FILE" <> help "The archive file to view its symbols")

parseOptionFileList :: Parser Command
parseOptionFileList = FileList
    <$> argument str (metavar "FILE" <> help "The archive file to view its entries")

parseOptionSymbolSearch :: Parser Command
parseOptionSymbolSearch = SymbolSearch
    <$> argument str (metavar "PATTERN" <> help "The pattern to match symbols for")
    <*> argument str (metavar "SPATH"   <> help "The path to search for archives")

parseCommand :: Parser Command
parseCommand = subparser $
    command "symbols" (parseOptionSymbolList `withInfo` "List symbols inside library")      <>
    command "entries" (parseOptionFileList `withInfo` "List files inside library")          <>
    command "search"  (parseOptionSymbolSearch `withInfo` "Search for symbol in libraries")

-- Main parser
parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

-- The main description generator
parseOptionsInfo :: ParserInfo Options
parseOptionsInfo = info (helper <*> parseOptions)
                        (fullDesc
                      <> header "HAr - A SysV/Gnu ar archive inspector")

---------------------------------------------------------------------------
-- FileSystem
---------------------------------------------------------------------------
-- A getDirectoryContents equivalent that returns empty list if exception thrown
safeGetDirectoryContents :: FilePath -> IO [FilePath]
safeGetDirectoryContents x = do
    result <- try (getDirectoryContents x) :: IO (Either SomeException [FilePath])
    case result of
        Left _ -> return []
        Right val -> return val

-- Returns a list of all the given directory contents recursively
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- safeGetDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- mapM (\x -> do
        let path = topdir </> x
        isDir <- doesDirectoryExist path
        if isDir
            then (++ [path]) <$> getRecursiveContents path
            else return [path]
        ) properNames
    return (concat paths)

-- Equivalent of getRecursiveContents but filters files only
getDirFiles :: FilePath -> IO [FilePath]
getDirFiles x = getRecursiveContents x >>= filterM doesFileExist

---------------------------------------------------------------------------
-- Printing
---------------------------------------------------------------------------
-- Reads archive from file with given filename, or errors on failure
tryParseArchive :: FilePath -> IO Archive
tryParseArchive f = do
    contents <- L.readFile f
    let archive = parseAr contents
    case archive of
        Left e  -> error $ "Error while parsing archive: " ++ e
        Right a -> return a

-- Tries to compile a regex from given pattern or errors on failure
checkValidRegexString :: String -> IO ()
checkValidRegexString s = do
    r <- compile compBlank execBlank s
    case r of
        Left (_, err) -> error $ "Error while parsing given regular expression: " ++ err
        Right _ -> return ()

-- Shows the symbols that match the given regex pattern inside an archive
showArMatchingSymbols :: String -> FilePath -> IO ()
showArMatchingSymbols pat f = do
    checkValidRegexString pat
    a <- tryParseArchive f
    let syms = symbols a
    let matches = filter (=~ pat) syms
    mapM_ (\m -> putStrLn $ f ++ ": " ++ m) matches
 
-- Prints the list with the symbols inside an archive
showArSymbols :: FilePath -> IO ()
showArSymbols f = do
    a <- tryParseArchive f
    let syms = symbols a
    mapM_ putStrLn syms

-- Prints the list with the file entries inside an archive
showArEntries :: FilePath -> IO ()
showArEntries f = do
    a <- tryParseArchive f
    let ens = entries a
    mapM_ putStrLn [fileName x | x <- ens]

---------------------------------------------------------------------------
-- Actions
---------------------------------------------------------------------------
-- Checks if given path points to a static library file
isValidStaticLibFilename :: FilePath -> Bool
isValidStaticLibFilename = (\x -> isPrefixOf "lib" x && isSuffixOf ".a" x) . takeFileName

-- Checks if given path is a file or fails with error message
checkIsFile :: String -> IO String
checkIsFile f = do
    isFile <- doesFileExist f
    if not isFile
        then error "Error: Given path is not a file"
        else return f

-- Lists symbols of the archive with the given filepath
symbolList :: String -> IO ()
symbolList f = showArSymbols =<< checkIsFile f

-- Lists entries of the archive with the given filepath
entryList :: String -> IO ()
entryList f = showArEntries =<< checkIsFile f

-- Searches for symbol that matches the given pattern
-- in the archives contained in the given directory
symbolSearch :: String -> FilePath -> IO ()
symbolSearch pattern dir = do
    contents <- getDirFiles dir
    let libs = filter isValidStaticLibFilename contents
    mapM_ (showArMatchingSymbols pattern) libs

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------
-- Program Entrypoint
main :: IO ()
main = do
    options <- execParser parseOptionsInfo
    let c = cmd options
    case c of
        SymbolList a     -> symbolList a
        FileList a       -> entryList a
        SymbolSearch p d -> symbolSearch p d

