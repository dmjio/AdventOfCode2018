{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Monad
import           Control.Monad.State
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.ByteString.Internal
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Function
import qualified Data.IntSet                as I
import           Debug.Trace
import           Text.Read

differences :: IO [Int -> Int]
differences = map toFunction
            . L8.lines <$> L.readFile "1.txt"
  where
    toFunction :: L8.ByteString -> Int -> Int
    toFunction (L.uncons -> Just (w2c -> h, readMaybe @Int . L8.unpack -> Just t)) = h `op` t

    op :: Char -> (Int -> Int -> Int)
    op '+' = (+)
    op '-' = subtract
    op c   = error (c:" : impossible")

main :: IO ()
main = do
  putStrLn "Solution to first part"
  diffs <- differences
  print $ foldl (&) 0 diffs

  putStrLn "Solution to second part"
  diffs <- differences
  print $ findFreq (I.singleton 0) 0 (cycle diffs)
    where
      findFreq imap !acc (f:fs) = do
        let freq = f acc
        if freq `I.member` imap
          then
            freq
          else
            findFreq (I.insert freq imap) freq fs
