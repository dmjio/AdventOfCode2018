{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S

main :: IO ()
main = do
  maps' <- map parseInput . lines <$> readFile "3.txt"
  let claimsMap = foldl' (M.unionWith (+)) M.empty maps'
  putStrLn "Answer to first part"
  print $ M.size $ M.filter (>1) claimsMap
  putStrLn "Answer to second part"
  print $ head $ map fst $ filter (pred claimsMap) (zip [1..] maps')
    where
      pred cmap (claim, map')
        = all (==True) $ do
            ((x,y),v) <- M.toList map'
            pure $ M.lookup (x,y) cmap == Just 1

parseInput :: [Char] -> M.Map (Int,Int) Int
parseInput xs =
  case splitOn " " xs of
    [read . drop 1 -> claim :: Int,_,nums,dims] ->
      case splitOn "," nums of
        [read -> x, read . init -> y] ->
          case splitOn "x" dims of
            [read -> h, read -> w] -> do
              M.fromList $ flip zip (repeat 1) $ do
                x' <- [0..h-1]
                y' <- [0..w-1]
                pure ( x' + x
                     , y' + y
                     )
