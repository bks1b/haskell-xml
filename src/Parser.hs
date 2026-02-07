module Parser (tokenize, parse) where

import Tags
import Util

import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Control.Monad

-- finds the first closing token which is not enclosed in quotes
closingToken :: String -> Bool -> ErrorMsg Int
closingToken ('\\' : xs) b = fmap (+ 2) $ closingToken (drop 1 xs) b
closingToken ('>' : _) False = Right 0
closingToken (x : xs) b = fmap (+ 1) $ closingToken xs $ b /= (x == '"')
closingToken _ _ = Left "Closing token expected of tag at"

-- splits an XML document into text between <> and text outside of <>, and removes comments
splitDocument :: Int -> String -> Error [Item]
splitDocument pos ('<' : '!' : '-' : '-' : xs) = case splitStr "-->" xs of
    Just (t, ts) -> splitDocument (pos + length t + 7) $ drop 3 ts
    _ -> Left ("Closing expected of comment at", pos)
splitDocument pos ('<' : xs) = fmap (split xs) (setPos pos $ closingToken xs False)
    >>= \(t, ts) -> splitDocument (pos + length t + 2) (drop 1 ts)
        >>= \x -> setPos pos $ splitTag t
            >>= fmap (: x) . setItemPos pos . parseTag
splitDocument _ [] = Right []
splitDocument pos x = fmap
    (defaultItem { itemType = Content, value = unescape t, position = pos } :)
    (splitDocument (pos + length t) ts)
    where (t, ts) = fromMaybe (x, []) $ splitStr "<" x

-- finds the first closing tag with the given name at a given depth
closingIdx :: [Item] -> String -> Int -> ErrorMsg Int
closingIdx (x : xs) name d = if value x == name && tagType x == End && d == 0
    then Right 0
    else fmap (+ 1) $ closingIdx xs name
        $ (d +) $ bool 0 (bool (-1) 1 $ tagType x == Start) $ value x == name
closingIdx _ _ _ = Left "Closing tag expected of tag at"

-- recursively groups Items between opening and closing tags
groupItems :: [Item] -> Error [Item]
groupItems (x@Item { tagType = Empty } : xs) = fmap (x :) $ groupItems xs
groupItems (x@Item { tagType = Start } : xs) = fmap (split xs)
    (setPos (position x) $ closingIdx xs (value x) 0)
    >>= \(t, ts) -> groupItems t
        >>= \c -> fmap (x { children = c } :) $ groupItems $ drop 1 ts
groupItems (x : _) = Left ("Unexpected closing tag at", position x)
groupItems x = Right x

-- wrapper for splitDocument
tokenize :: String -> ErrorMsg [Item]
tokenize = formatError <*> splitDocument 0

-- splits an XML document into Items and groups them
parse :: String -> ErrorMsg [Item]
parse = ap formatError $ groupItems <=< splitDocument 0