-- |
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Bits
import Data.Int
import Data.Proxy
import Data.Word
import Numeric.QuoteQuot
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Printf

#ifdef MIN_VERSION_word24
import Data.Int.Int24
import Data.Word.Word24
#endif

#ifdef MIN_VERSION_wide_word
import Data.WideWord
#endif

main :: IO ()
main = defaultMain $ testGroup "All" [testAst, testQuotes]

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
  , testProperty "below zero" (prop @a . negate . getNonNegative)
  , testProperty "above minBound" (prop @a . (minBound +) . getNonNegative)
  , testProperty "below maxBound" (prop @a . (maxBound -) . getNonNegative)
  ]
  | otherwise =
  [ testProperty "above zero" (prop @a)
  , testProperty "below maxBound" (prop @a . (maxBound -))
  ]

prop
  :: (Integral a, FiniteBits a, Show a)
  => a -> Positive a -> Property
prop x (Positive y) = counterexample
  (printf
    "%s `quot` %s = %s /= %s = eval (%s) %s"
    (show x) (show y) (show ref) (show q) (show ast) (show x))
  (q == ref)
  where
    ref = x `quot` y
    ast = astQuot y
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

testQuotes :: TestTree
testQuotes = testGroup "Quotes"
  [ testProperty  "1" $ \x -> $$(quoteQuotRem  1) x === x `quotRem`  1
  , testProperty  "2" $ \x -> $$(quoteQuotRem  2) x === x `quotRem`  2
  , testProperty  "3" $ \x -> $$(quoteQuotRem  3) x === x `quotRem`  3
  , testProperty  "4" $ \x -> $$(quoteQuotRem  4) x === x `quotRem`  4
  , testProperty  "5" $ \x -> $$(quoteQuotRem  5) x === x `quotRem`  5
  , testProperty  "6" $ \x -> $$(quoteQuotRem  6) x === x `quotRem`  6
  , testProperty  "7" $ \x -> $$(quoteQuotRem  7) x === x `quotRem`  7
  , testProperty  "8" $ \x -> $$(quoteQuotRem  8) x === x `quotRem`  8
  , testProperty  "9" $ \x -> $$(quoteQuotRem  9) x === x `quotRem`  9
  , testProperty "10" $ \x -> $$(quoteQuotRem 10) x === x `quotRem` 10
  , testProperty "maxBound" $ \x -> $$(quoteQuotRem maxBound) x === x `quotRem` maxBound
  , testProperty "maxBound - 1" $ \x -> $$(quoteQuotRem (maxBound - 1)) x === x `quotRem` (maxBound - 1)
  ]
