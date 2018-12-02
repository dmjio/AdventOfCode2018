{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Data.List
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  lines <- lines <$> readFile "2.txt"
  putStrLn "Solution to part 1"
  print $ foldl' (go 2) 0 lines * foldl' (go 3) 0 lines
  putStrLn "Solution to part 2"
  mapM_ findBox $ dupli $ sort lines

dupli :: [a] -> [(a, a)]
dupli (x:y:xs) = (x,y) : dupli (y:xs)
dupli _ = []

findBox :: (String, String) -> IO ()
findBox (x,y) | editDistance == 1 = putStrLn matches
              | otherwise = pure ()
  where
    editDistance = length $ filter (==False) (zipWith (==) x y)
    matches = map fst $ filter (uncurry (==)) (zip x y)

go :: Int -> Int -> String -> Int
go k acc = (acc+) . count k . list

list :: String -> M.Map Char Int
list = M.fromListWith (+) . flip zip (repeat 1)

count :: Int -> M.Map Char Int -> Int
count k ( length
        . filter (==k)
        . nub
        . M.elems -> n
        ) = n

