module Util where

import Data.List
import Data.Bifunctor
import Control.Arrow ((&&&))

type ErrorMsg = Either String
type Error = Either (String, Int)

data ItemType = Tag | Content deriving (Show, Eq)
data TagType = Start | End | Empty deriving Eq
data Item = Item {
    itemType :: ItemType,
    tagType :: TagType,
    value :: String,
    attribs :: [[String]],
    children :: [Item],
    position :: Int
}

defaultItem = Item {
    itemType = Tag,
    tagType = Empty,
    value = [],
    attribs = [],
    children = [],
    position = 0
}

setPos :: Int -> ErrorMsg a -> Error a
setPos = first . flip (,)

setItemPos :: Functor f => Int -> f Item -> f Item
setItemPos p = fmap (\x -> x { position = p })

indent = "    "

-- displays an Item with indentation
showItem :: Item -> [String]
showItem x = flip (:) (concatMap (map (indent ++) . showItem) $ children x) $ (++)
    (show (itemType x) ++ ": " ++ show (map (\c -> if c == '\n' then ' ' else c) $ value x))
    (if itemType x == Tag then "; " ++ show (attribs x) ++ "\n" else "\n")

instance Show Item where
    show = concat . showItem

formatError :: String -> Error a -> ErrorMsg a
formatError = first . (uncurry (++) .) . fmap . getPosition (0, 0) 0

getPosition :: (Int, Int) -> Int -> String -> Int -> String
getPosition (r, c) j _ i | i == j = " line " ++ show (r + 1) ++ ", column " ++ show (c + 1) ++ " (character " ++ show i ++ ")"
getPosition (r, c) j (x : xs) i = getPosition (if x == '\n' then (r + 1, 0) else (r, c + 1)) (j + 1) xs i

unescape :: String -> String
unescape = filter (/= '\\')

split :: [a] -> Int -> ([a], [a])
split = flip $ curry $ uncurry take &&& uncurry drop

-- splits a string at the first occurrence of another string, ignoring escaped characters
splitStr :: String -> String -> Maybe (String, String)
splitStr str = fmap (first reverse)
    . find (uncurry (&&) . bimap (liftA2 (||) null $ (/= '\\') . (!! 0)) (isPrefixOf str))
    . (zip . reverse . tails . reverse <*> tails)