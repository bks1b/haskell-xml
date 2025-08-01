module Util where

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

-- displays an Item with indentation
showItem :: Item -> [String]
showItem x =
    (
        (show $ itemType x) ++ ": " ++ (show $ map (\c -> if c == '\n' then ' ' else c) (value x))
        ++ (if itemType x == Tag then "; " ++ (show $ attribs x) else "") ++ "\n"
    ) : concat (map (\c -> map ("    " ++) (showItem c)) (children x))

instance Show Item where
    show x = concat $ showItem x

formatError :: String -> Error a -> Either String a
formatError s x = case x of
    Right r -> Right r
    Left (l, i) -> Left $ l ++ (getPosition (0, 0) 0 s i)

getPosition :: (Int, Int) -> Int -> String -> Int -> String
getPosition (r, c) j _ i | i == j = " line " ++ (show (r + 1)) ++ ", column " ++ (show (c + 1)) ++ " (character " ++ (show i) ++ ")"
getPosition (r, c) j (x : xs) i = getPosition (if x == '\n' then (r + 1, 0) else (r, c + 1)) (j + 1) xs i

-- removes escaping characters
unescape :: String -> String
unescape "" = ""
unescape ('\\' : x : xs) = unescape (x : xs)
unescape (x : xs) = x : unescape xs

-- finds the first index of a char in a string (or returns the length), ignoring escaped characters
indexOf :: String -> Char -> Int -> Int
indexOf "" _ d = d
indexOf ('\\' : xs) c d = indexOf (drop 1 xs) c (d + 2)
indexOf (x : xs) c d = if x == c then d else indexOf xs c (d + 1)

-- ignores escaped characters
contains :: String -> Char -> Bool
contains str c = (indexOf str c 0) < (length str)

-- partitions a list at an index
split :: [a] -> Int -> [[a]]
split a i = map (\f -> f i a) [take, drop]

-- splits a string at the first occurrence of a character, ignoring escaped characters
splitStr :: String -> Char -> [String]
splitStr str c = split str (indexOf str c 0)

-- for composed fmaps
decomposeF :: Either a (Either a b) -> Either a b
decomposeF (Left l) = Left l
decomposeF (Right r) = r