module Main where

import System.Directory
import Data.List
import Parser
import Util

-- splits a string at each occurrence n newlines
splitLines :: String -> Int -> [String]
splitLines x n = case find (\i -> (take n $ drop i x) == (concat $ replicate n "\n")) [0..length x] of
    Just i -> [t, drop n ts] where [t, ts] = split x i
    Nothing -> [x, ""]

splitTests :: String -> [[String]]
splitTests "" = []
splitTests x = splitLines t 1 : splitTests ts where [t, ts] = splitLines x 3

runTest :: String -> String -> Int -> IO ()
runTest name str i = do
    putStrLn $ "Parsing test #" ++ (show i) ++ ": " ++ name
    case parse str of
        Right r -> putStrLn $ concat $ map show r
        Left l -> putStrLn l
    putStrLn ""

main = do
    file <- readFile "tests.txt"
    let tests = splitTests file
    let added = map (\t -> (tests !! 0 !! 1) ++ "\n\n" ++ (t !! 1)) tests
    createDirectoryIfMissing False "tests"
    sequence [
        writeFile ("tests/" ++ (show i) ++ ".xml") (added !! i)
        >> runTest (tests !! i !! 0) (added !! i) i
        | i <- [1..length tests - 1]]