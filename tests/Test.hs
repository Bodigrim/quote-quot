-- |
-- Copyright:   (c) 2020-2022 Andrew Lelechenko
-- Licence:     BSD3
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

#include "MachDeps.h"

module Main (main) where

import Data.Bits
import Data.Int
import Data.Proxy
import Data.Word
import Numeric.QuoteQuot
import Test.Tasty
import Test.Tasty.QuickCheck

#ifdef MIN_VERSION_word24
import Data.Int.Int24
import Data.Word.Word24
#endif

#ifdef MIN_VERSION_wide_word
import Data.WideWord
#endif

main :: IO ()
main = defaultMain $ testGroup "All" [testAst, testQuotes, testMulHi]

testAst :: TestTree
testAst = testGroup "Ast"
  [ testGroup "Word"    (mkTests (Proxy @Word))
  , testGroup "Word8"   (mkTests (Proxy @Word8))
  , testGroup "Word16"  (mkTests (Proxy @Word16))
  , testGroup "Word32"  (mkTests (Proxy @Word32))
  , testGroup "Word64"  (mkTests (Proxy @Word64))
  , testGroup "Int"     (mkTests (Proxy @Int))
  , testGroup "Int8"    (mkTests (Proxy @Int8))
  , testGroup "Int16"   (mkTests (Proxy @Int16))
  , testGroup "Int32"   (mkTests (Proxy @Int32))
  , testGroup "Int64"   (mkTests (Proxy @Int64))
#ifdef MIN_VERSION_word24
  , testGroup "Word24"  (mkTests (Proxy @Word24))
  , testGroup "Int24"   (mkTests (Proxy @Int24))
#endif
#ifdef MIN_VERSION_wide_word
  , testGroup "Word128" (mkTests (Proxy @Word128))
  , testGroup "Word256" (mkTests (Proxy @Word256))
  , testGroup "Int128"  (mkTests (Proxy @Int128))
#endif
  ]

mkTests
  :: forall a.
     (Integral a, FiniteBits a, Show a, Bounded a, Arbitrary a)
  => Proxy a -> [TestTree]
mkTests _
  | isSigned (undefined :: a) =
  [ testProperty "above zero" (prop @a . getNonNegative)
  , testProperty "above zero assumeNonNegArg" (propNonNeg @a)
  , testProperty "below zero" (prop @a . negate . getNonNegative)
  , testProperty "above minBound" (prop @a . (minBound +) . getNonNegative)
  , testProperty "below maxBound" (prop @a . (maxBound -) . getNonNegative)
  , testProperty "below maxBound assumeNonNegArg" (propNonNeg @a . NonNegative . (maxBound -) . getNonNegative)
  ]
  | otherwise =
  [ testProperty "above zero" (prop @a)
  , testProperty "below maxBound" (prop @a . (maxBound -))
  ]

prop
  :: (Integral a, FiniteBits a, Show a)
  => a -> Positive a -> Property
prop x (Positive y) =
  q === ref
  where
    ref = x `quot` y
    ast = astQuot y
    q   = interpretAST ast x

propNonNeg
  :: (Integral a, FiniteBits a, Show a)
  => NonNegative a -> Positive a -> Property
propNonNeg (NonNegative x) (Positive y) =
  q === ref
  where
    ref = x `quot` y
    ast = assumeNonNegArg $ astQuot y
    q   = interpretAST ast x

#ifdef MIN_VERSION_word24
instance Arbitrary Word24  where arbitrary = arbitrarySizedBoundedIntegral
instance Arbitrary Int24   where arbitrary = arbitrarySizedBoundedIntegral
#endif

#ifdef MIN_VERSION_wide_word
instance Arbitrary Word128 where arbitrary = arbitrarySizedBoundedIntegral
instance Arbitrary Word256 where arbitrary = arbitrarySizedBoundedIntegral
instance Arbitrary Int128  where arbitrary = arbitrarySizedBoundedIntegral
#endif

#define testQuotes(ty) \
    [ testProperty  "1" $ \x -> $$(quoteQuotRem  (1 :: ty)) x === x `quotRem`  1 \
    , testProperty  "2" $ \x -> $$(quoteQuotRem  (2 :: ty)) x === x `quotRem`  2 \
    , testProperty  "3" $ \x -> $$(quoteQuotRem  (3 :: ty)) x === x `quotRem`  3 \
    , testProperty  "4" $ \x -> $$(quoteQuotRem  (4 :: ty)) x === x `quotRem`  4 \
    , testProperty  "5" $ \x -> $$(quoteQuotRem  (5 :: ty)) x === x `quotRem`  5 \
    , testProperty  "6" $ \x -> $$(quoteQuotRem  (6 :: ty)) x === x `quotRem`  6 \
    , testProperty  "7" $ \x -> $$(quoteQuotRem  (7 :: ty)) x === x `quotRem`  7 \
    , testProperty  "8" $ \x -> $$(quoteQuotRem  (8 :: ty)) x === x `quotRem`  8 \
    , testProperty  "9" $ \x -> $$(quoteQuotRem  (9 :: ty)) x === x `quotRem`  9 \
    , testProperty "10" $ \x -> $$(quoteQuotRem (10 :: ty)) x === x `quotRem` 10 \
    , testProperty "maxBound" $ \x -> $$(quoteQuotRem (maxBound :: ty)) x === x `quotRem` maxBound \
    , testProperty "maxBound - 1" $ \x -> $$(quoteQuotRem (maxBound - 1 :: ty)) x === x `quotRem` (maxBound - 1) \
    ] \

testQuotes :: TestTree
testQuotes = testGroup "Quotes"
  [ testGroup "Word8"  testQuotes(Word8)
  , testGroup "Word16" testQuotes(Word16)
  , testGroup "Word32" testQuotes(Word32)
  , testGroup "Word64" testQuotes(Word64)
  , testGroup "Word"   testQuotes(Word)
  , testGroup "Int8"   testQuotes(Int8)
  , testGroup "Int16"  testQuotes(Int16)
  , testGroup "Int32"  testQuotes(Int32)
  , testGroup "Int64"  testQuotes(Int64)
  , testGroup "Int"    testQuotes(Int)
  ]

testMulHi :: TestTree
testMulHi = testGroup "MulHi"
  [ testGroup "Word"    (mkTestsMulHi (Proxy @Word))
  , testGroup "Word8"   (mkTestsMulHi (Proxy @Word8))
  , testGroup "Word16"  (mkTestsMulHi (Proxy @Word16))
  , testGroup "Word32"  (mkTestsMulHi (Proxy @Word32))
  , testGroup "Word64"  (mkTestsMulHi (Proxy @Word64))
  , testGroup "Int"     (mkTestsMulHi (Proxy @Int))
  , testGroup "Int8"    (mkTestsMulHi (Proxy @Int8))
  , testGroup "Int16"   (mkTestsMulHi (Proxy @Int16))
  , testGroup "Int32"   (mkTestsMulHi (Proxy @Int32))
  , testGroup "Int64"   (mkTestsMulHi (Proxy @Int64))
  ]

mkTestsMulHi
  :: forall a.
     (MulHi a, Show a, Bounded a, Arbitrary a)
  => Proxy a -> [TestTree]
mkTestsMulHi _ = [ combineMods mod1 mod2 | mod1 <- mods, mod2 <- mods]
  where
    mods :: [(String, a -> Bool, a -> a)]
    mods
      | isSigned (undefined :: a) =
      [ ("above zero", (>= 0), id)
      , ("below zero", (>= 0), negate)
      , ("above minBound", (>= 0), (minBound +))
      , ("below maxBound", (>= 0), (maxBound -))
      ]
      | otherwise =
      [ ("above zero", const True, id)
      , ("below maxBound", const True, (maxBound -))
      ]

    combineMods (name1, pred1, tr1) (name2, pred2, tr2) =
      testProperty (name1 ++ ", " ++ name2) $
        \x y -> pred1 x && pred2 y ==> propMulHi (tr1 x) (tr2 y)

propMulHi :: (MulHi a, Show a) => a -> a -> Property
propMulHi x y = mulHi x y ===
  fromInteger ((toInteger x * toInteger y) `shiftR` finiteBitSize x)
