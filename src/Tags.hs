module Tags where

import Util

import Data.List
import Data.Maybe (isJust)
import Data.Bool (bool)
import Control.Monad (join)

tokens = ["\"", " \n", "/="]

-- splits a tag into tokens
splitTag :: String -> ErrorMsg [String]
splitTag ('"' : xs) = case splitStr (tokens !! 0) xs of
    Just (t, ts) -> fmap (unescape t :) $ splitTag $ drop 1 ts
    _ -> Left "String closing expected in tag at" 
splitTag a@(x : xs) = if wspace || elem x (tokens !! 2)
    then fmap (bool ([x] :) id wspace) $ splitTag xs
    else case join $ find isJust $ map (flip splitStr a . (: [])) $ concat tokens of
        Just (t, ts) -> splitTag t >>= flip fmap (splitTag ts) . (++)
        _ -> Right [a]
    where wspace = elem x (tokens !! 1)
splitTag _ = Right []

-- creates an Item from a list of tokens
parseTag :: [String] -> ErrorMsg Item
parseTag ("/" : xs) = case xs of
    ["/"] -> Left "Unexpected token / of tag at"
    [x] -> Right defaultItem { tagType = End, value = x }
    [] -> Left "Name expected of tag at"
    _ -> Left "Unexpected tokens of closing tag at"
parseTag (x : xs) = case elemIndex "/" xs of
    Just i | i + 1 < length xs -> Left "Unexpected token / of tag at"
    v -> fmap
        (\a -> defaultItem { tagType = bool Start Empty $ isJust v, value = x, attribs = a })
        (parseAttribs $ (bool id init $ isJust v) xs)
parseTag _ = Left "Invalid empty tag at"

-- parses attributes from a list of tokens
parseAttribs :: [String] -> ErrorMsg [[String]]
parseAttribs (k : "=" : v : xs) = fmap ([k, v] :) $ parseAttribs xs
parseAttribs (k : xs) = if elem k ["=", "/"]
    then Left $ "Unexpected token " ++ k ++ " of tag at"
    else fmap ([k] :) $ parseAttribs xs
parseAttribs _ = Right []