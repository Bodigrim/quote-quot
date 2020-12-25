-- |
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
--

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.List
import Numeric.QuoteQuot
import System.CPUTime

measure :: String -> (Word -> Word) -> IO ()
measure name f = do
  t0 <- getCPUTime
  print $ foldl' (+) 0 $ map f [0..100000000]
  t1 <- getCPUTime
  putStrLn $ name ++ " " ++ show ((t1 - t0) `quot` 1000000000) ++ " ms"
{-# INLINE measure #-}

main :: IO ()
main = do
  measure "     (`quot` 10)"      (`quot` 10)
  measure "$$(quoteQuot 10)" $$(quoteQuot 10)
