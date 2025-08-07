-- |
-- Copyright:   (c) 2020-2022 Andrew Lelechenko
-- Licence:     BSD3
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

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
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Word) ++ "\" && $(NF-2) == \"Word\"") \
    $ measureWord "quoteQuot" $$(quoteQuot (n :: Word)) \
    ]

#define benchInt(n) \
  bgroup (show (n :: Int)) \
    [ measureInt "quot" (`quot` (n :: Int)) \
    , bcompare ("$NF == \"quot\" && $(NF-1) == \"" ++ show (n :: Int) ++ "\" && $(NF-2) == \"Int\"") \
    $ measureInt "quoteQuot" $$(quoteQuot (n :: Int)) \
    ]

#define benchDivInt(n) \
  bgroup (show (n :: Int)) \
    [ measureInt "div" (`div` (n :: Int)) \
    , bcompare ("$NF == \"div\" && $(NF-1) == \"" ++ show (n :: Int) ++ "\" && $(NF-2) == \"Int\"") \
    $ measureInt "quoteDiv" $$(quoteDiv (n :: Int)) \
    ]

main :: IO ()
main = defaultMain
  [ bgroup "Word"
    [ benchWord(3)
    , benchWord(5)
    , benchWord(7)
    , benchWord(10000)
    ]
#if MIN_VERSION_base(4,15,0)
  , bgroup "Int"
    [ benchInt(3)
    , benchInt(5)
    , benchInt(7)
    , benchInt(10000)
    ]
  , bgroup "DivInt"
    [ benchDivInt(3)
    , benchDivInt(5)
    , benchDivInt(7)
    , benchDivInt(10000)
    ]
#endif
  ]
