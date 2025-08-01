module Tags where

import Data.List
import Util

-- splits a tag into tokens
splitTag :: String -> Int -> Error [String]
splitTag "" _ = Right []
splitTag ('"' : xs) pos = if length ts > 0
    then fmap
        (unescape t :)
        $ splitTag (drop 1 ts) pos
    else Left ("String closing expected in tag at", pos)
    where [t, ts] = splitStr xs '"'
splitTag a@(x : xs) pos = if elem x "/= \n"
    then fmap
        ((if elem x " \n" then [] else [[x]]) ++)
        $ splitTag xs pos
    else case (find (\c -> contains a c) "\" \n/=") of
        Nothing -> Right [a]
        Just c -> decomposeF $ fmap
            (\r -> fmap (r ++) $ splitTag ts pos)
            $ splitTag t pos
            where [t, ts] = splitStr a c

-- creates an Item from a list of tokens
parseTag :: [String] -> Int -> Error Item
parseTag ("/" : x : []) pos = if x == "/"
    then parseTag ["/"] pos
    else Right Item {
        itemType = Tag,
        tagType = End,
        value = x,
        attribs = [],
        children = [],
        position = pos
    }
parseTag ("/" : x : _) pos = Left ("Unexpected tokens of closing tag at", pos)
parseTag ["/"] pos = Left ("Name expected of tag at", pos)
parseTag (x : xs) pos = case (elemIndex "/" xs) of
    Just j -> if j + 1 < length xs
        then Left ("Unexpected token / of tag at", pos)
        else fmap
            (\a -> Item {
                itemType = Tag,
                tagType = Empty,
                value = x,
                attribs = a,
                children = [],
                position = pos
            })
            $ parseAttribs (take (length xs - 1) xs) pos
    Nothing -> fmap
        (\a -> Item {
            itemType = Tag,
            tagType = Start,
            value = x,
            attribs = a,
            children = [],
            position = pos
        })
        $ parseAttribs xs pos
parseTag [] pos = Left ("Invalid empty tag at", pos)

-- parses attributes from a list of tokens
parseAttribs :: [String] -> Int -> Error [[String]]
parseAttribs [] _ = Right []
parseAttribs (k : "=" : v : xs) pos = fmap ([k, v] :) $ parseAttribs xs pos
parseAttribs (k : xs) pos = if elem k ["=", "/"]
    then Left ("Unexpected token " ++ k ++ " of tag at", pos)
    else fmap ([k] :) $ parseAttribs xs pos