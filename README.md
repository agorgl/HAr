HAr
===

Introduction
------------
HAr is a command line tool for inspecting GNU/System V compliant ar archive files written in Haskell

Features
--------
 * Viewing the symbols of the archive
 * Listing the files contained in the archive
 * Searching for a symbol among multiple libraries

Building
--------
 1. Clone the project and cd to its directory.
 2. Run:  
    ```
    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    ```
 3. Built binary will reside in the `dist\build\har` subdirectory.

Usage
-----
 * See `har --help` for options.

ChangeLog
---------
 * v0.0.1: Initial release

Contributing
------------
 * For bug fixes, any well checked pull requests are welcome

Credits
-------
Written and maintained by: 
* "Agorgianitis Loukas" <agorglouk@gmail.com>

Licensing
---------
Read [LICENSE](LICENSE.md)  

Copyright (C) 2015 Agorgianitis Loukas <agorglouk@gmail.com>  
All rights reserved.
