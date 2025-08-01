This is an XML parser I wrote in order to relearn Haskell.

# Syntax

The parser does not follow any specification. CDATA and character references are not implemented, and rules are generally loose.
See the first two blocks of the [test file](https://github.com/bks1b/haskell-xml/blob/main/tests.txt) for examples which get parsed without errors:

<img src="https://raw.githubusercontent.com/bks1b/haskell-xml/main/readme/result.png">

# Library

`Parser` module:
- `parse :: String -> Either String [Item]` parses an XML document
- `tokenize :: String -> Either String [Item]` tokenizes an XML document (separates tags and content without creating hierarchy)

# Tests

Run `ghc -isrc -odir build -hidir build -o build/Test.exe src/Test.hs && .\build\Test.exe` to parse the tests in `tests.txt`.
Blocks are separated by `\n\n\n`, and the first line of each block is a note. Each block is appended to the first block in the file, and written to a file in `/tests`.