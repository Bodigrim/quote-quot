-- |
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import GHC.Exts
import Numeric.QuoteQuot
import Test.Tasty.Bench

measureWord :: String -> (Word -> Word) -> Benchmark
measureWord name f = bench name $
  whnf (\(W# n#) -> W# (go 0## n#)) 100000000
  where
    go acc# 0## = acc#
    go acc# k# = go (let !(W# fk) = f (W# k#) in acc# `plusWord#` fk) (k# `minusWord#` 1##)
{-# INLINE measureWord #-}

measureInt :: String -> (Int -> Int) -> Benchmark
measureInt name f = bench name $
  whnf (\(I# n#) -> I# (go 0# n#)) 100000000
  where
    go acc# 0# = acc#
    go acc# k# = go (let !(I# fk) = f (I# k#) in acc# +# fk) (k# -# 1#)
{-# INLINE measureInt #-}

#define benchWord(n) \
  bgroup (show (n :: Word)) \
    [ measureWord "quot" (`quot` (n :: Word)) \
    , measureWord "quoteQuot" $$(quoteQuot (n :: Word)) \
    ]

#define benchInt(n) \
  bgroup (show (n :: Int)) \
    [ measureInt "quot" (`quot` (n :: Int)) \
    , measureInt "quoteQuot" $$(quoteQuot (n :: Int)) \
    ]

main :: IO ()
main = defaultMain
  [ bgroup "Word"
    [ benchWord(3)
    , benchWord(5)
    , benchWord(7)
    ]
#if MIN_VERSION_base(4,15,0)
  , bgroup "Int"
    [ benchInt(3)
    , benchInt(5)
    , benchInt(7)
    ]
#endif
  ]
