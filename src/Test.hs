import Parser

import Data.List
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap)
import System.Directory (createDirectoryIfMissing)

-- splits a string at each occurrence of n newlines
splitLines :: Int -> String -> (String, String)
splitLines n = fmap (drop n) . fromJust
    . find (liftA2 (||) null (isPrefixOf $ replicate n '\n') . snd)
    . (zip . inits <*> tails)

splitTests :: String -> [(String, String)]
splitTests [] = []
splitTests x = uncurry (:) $ bimap (splitLines 1) splitTests $ splitLines 3 x

writeTest :: Int -> String -> IO ()
writeTest = writeFile . ("tests/" ++) . (++ ".xml") . show 

runTest :: (Int, (String, String)) -> IO ()
runTest (i, (desc, str)) = mapM_ putStrLn [
    "Parsing test #" ++ show i ++ ": " ++ desc,
    either id (concatMap show) $ parse str,
    ""]

main = createDirectoryIfMissing False "tests"
    >> readFile "tests.txt"
    >>= uncurry (\(_, pref) -> mapM_ (
        liftA2 (>>) (uncurry writeTest . fmap snd) runTest
            . fmap (fmap $ (pref ++) . ("\n\n" ++)))
            . zip [1..]) . fromJust . uncons . splitTests