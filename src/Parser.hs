module Parser where

import Data.List
import Util
import Tags

-- finds the first closing token which is not enclosed in quotes
closingToken :: String -> Int -> Int -> Bool -> Error Int
closingToken ('\\' : xs) pos i b = closingToken (drop 1 xs) pos (i + 2) b
closingToken ('"' : xs) pos i b = closingToken xs pos (i + 1) (not b)
closingToken ('>' : _) _ i False = Right i
closingToken (_ : xs) pos i b = closingToken xs pos (i + 1) b
closingToken "" pos _ _ = Left ("Closing token expected of tag at", pos)

-- splits an XML document into text between <> and text outside of <>, and removes comments
splitDocument :: Int -> String -> Error [Item]
splitDocument _ "" = Right []
splitDocument pos ('<' : xs) = if take 3 xs == "!--"
    then case find (\i -> (take 3 $ drop i xs) == "-->" && (take 1 $ drop (i - 1) xs) /= "\\") [3..length xs] of
        Just i -> splitDocument (pos + i + 4) (drop (i + 3) xs)
        Nothing -> Left ("Closing expected of comment at", pos)
    else decomposeF $ decomposeF $ decomposeF $ fmap
        (\token -> let [t, ts] = split xs token in fmap
            (\x -> fmap
                (\e -> fmap (: x) $ parseTag e pos)
                $ splitTag t pos
            )
            $ splitDocument (pos + length t + 2) (drop 1 ts)
        )
        $ closingToken xs pos 0 False
splitDocument pos x = fmap
    (Item {
        itemType = Content,
        tagType = Empty,
        value = unescape t,
        attribs = [],
        children = [],
        position = pos
    } :)
    $ splitDocument (pos + length t) ts
    where [t, ts] = splitStr x '<'

-- finds the first closing tag with the given name at a given depth
closingIdx :: [Item] -> String -> Int -> Int -> Int -> Error Int
closingIdx [] _ pos _ _ = Left ("Closing tag expected of tag at", pos)
closingIdx (x : xs) name pos i d = if (value x) == name
    then (if (tagType x) == Start
        then closingIdx xs name pos (i + 1) (d + 1)
        else (if d == 0
            then Right i
            else closingIdx xs name pos (i + 1) (d - 1)))
    else closingIdx xs name pos (i + 1) d

-- recursively groups Items between opening and closing tags
groupItems :: [Item] -> Error [Item]
groupItems a@(Item { tagType = Empty } : xs) = fmap ((a !! 0) :) $ groupItems xs
groupItems a@(Item { tagType = Start } : xs) = decomposeF $ decomposeF $ fmap
    (\ci -> let [t, ts] = split xs ci in fmap
        (\c -> fmap
            (((a !! 0) { children = c }) :)
            $ groupItems (drop 1 ts)
        )
        $ groupItems t
    )
    (closingIdx xs (value (a !! 0)) (position $ a !! 0) 0 0)
groupItems [] = Right []
groupItems (x : _) = Left ("Unexpected closing tag at", position x)

-- wrapper for splitDocument
tokenize :: String -> Either String [Item]
tokenize x = formatError x $ splitDocument 0 x

-- splits an XML document into Items and groups them
parse :: String -> Either String [Item]
parse x = formatError x $ decomposeF $ fmap groupItems $ splitDocument 0 x