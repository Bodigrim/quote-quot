-- |
-- Module:      Numeric.QuoteQuot
-- Copyright:   (c) 2020-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Generate routines for integer division, employing arithmetic
-- and bitwise operations only, which are __2.5x-3.5x faster__
-- than 'quot'. Divisors must be known in compile-time and be positive.
--

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Numeric.QuoteQuot
  (
  -- * Quasiquoters
    quoteQuot
  , quoteRem
  , quoteQuotRem
  -- * AST
  , astQuot
  , AST(..)
  , interpretAST
  , quoteAST
  , assumeNonNegArg
  , MulHi(..)
  ) where

import Prelude
import Data.Bits
import Data.Int
import Data.Word
import GHC.Exts

-- | Quote integer division ('quot') by a compile-time known divisor,
-- which generates source code, employing arithmetic and bitwise operations only.
-- This is usually __2.5x-3.5x faster__ than using normal 'quot'.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# OPTIONS_GHC -ddump-splices -ddump-simpl -dsuppress-all #-}
-- > module Example where
-- > import Numeric.QuoteQuot
-- >
-- > -- Equivalent to (`quot` 10).
-- > quot10 :: Word -> Word
-- > quot10 = $$(quoteQuot 10)
--
-- >>> quot10 123
-- 12
--
-- Here @-ddump-splices@ demonstrates the chosen implementation
-- for division by 10:
--
-- > Splicing expression quoteQuot 10 ======>
-- > ((`shiftR` 3) . ((\ (W# w_a9N4) ->
-- >   let !(# hi_a9N5, _ #) = (timesWord2# w_a9N4) 14757395258967641293##
-- >   in W# hi_a9N5) . id))
--
-- And @-ddump-simpl@ demonstrates generated Core:
--
-- > quot10 = \ x_a5t2 ->
-- >   case x_a5t2 of { W# w_acHY ->
-- >   case timesWord2# w_acHY 14757395258967641293## of
-- >   { (# hi_acIg, ds_dcIs #) ->
-- >   W# (uncheckedShiftRL# hi_acIg 3#)
-- >   }
-- >   }
--
-- Benchmarks show that this implementation is __3.5x faster__
-- than @(`@'quot'@` 10)@.
--
quoteQuot ::
  _ => a -> _ (a -> a)
quoteQuot d = quoteAST (astQuot d)

-- | Similar to 'quoteQuot', but for 'rem'.
quoteRem ::
  _ => a -> _ (a -> a)
quoteRem d = [|| snd . $$(quoteQuotRem d) ||]

-- | Similar to 'quoteQuot', but for 'quotRem'.
quoteQuotRem ::
  _ => a -> _ (a -> (a, a))
quoteQuotRem d = [|| \w -> let q = $$(quoteQuot d) w in (q, w - d * q) ||]

-- | Types allowing to multiply wide and return the high word of result.
class (Integral a, FiniteBits a) => MulHi a where
  mulHi :: a -> a -> a

instance MulHi Word8 where
  mulHi x y = fromIntegral ((fromIntegral x * fromIntegral y :: Word16) `shiftR` 8)

instance MulHi Word16 where
  mulHi x y = fromIntegral ((fromIntegral x * fromIntegral y :: Word32) `shiftR` 16)

instance MulHi Word32 where
  mulHi x y = fromIntegral ((fromIntegral x * fromIntegral y :: Word64) `shiftR` 32)

-- | This instance is not efficient on 32-bit architecture.
instance MulHi Word64 where
  mulHi x y
    | finiteBitSize (0 :: Word) == 64
    = fromIntegral (fromIntegral x `mulHi` fromIntegral y :: Word)
    | otherwise
    = defaultMulHi x y

instance MulHi Word where
  mulHi (W# x) (W# y) = let !(# hi, _ #) = timesWord2# x y in W# hi

instance MulHi Int8 where
  mulHi x y = fromIntegral ((fromIntegral x * fromIntegral y :: Int16) `shiftR` 8)

instance MulHi Int16 where
  mulHi x y = fromIntegral ((fromIntegral x * fromIntegral y :: Int32) `shiftR` 16)

instance MulHi Int32 where
  mulHi x y = fromIntegral ((fromIntegral x * fromIntegral y :: Int64) `shiftR` 32)

-- | This instance is not efficient on 32-bit architecture.
instance MulHi Int64 where
  mulHi x y
    | finiteBitSize (0 :: Int) == 64
    = fromIntegral (fromIntegral x `mulHi` fromIntegral y :: Int)
    | otherwise
    = defaultMulHi x y

instance MulHi Int where
  mulHi (I# x) (I# y) = let !(# _, hi, _ #) = timesInt2# x y in I# hi

-- | An abstract syntax tree to represent
-- a function of one argument.
data AST a
  = Arg
  -- ^ Argument of the function
  | MulHi (AST a) a
  -- ^ Multiply wide and return the high word of result
  | MulLo (AST a) a
  -- ^ Multiply
  | Add (AST a) (AST a)
  -- ^ Add
  | Sub (AST a) (AST a)
  -- ^ Subtract
  | Shl (AST a) Int
  -- ^ Shift left
  | Shr (AST a) Int
  -- ^ Shift right with sign extension
  | CmpGE  (AST a) a
  -- ^ 1 if greater than or equal, 0 otherwise
  | CmpLT  (AST a) a
  -- ^ 1 if less than, 0 otherwise
  deriving (Show)

-- | Optimize 'AST', assuming that 'Arg' is non-negative.
assumeNonNegArg :: (Ord a, Num a) => AST a -> AST a
assumeNonNegArg = \case
  Add x (CmpLT Arg n)
    | n <= 0 -> x
  Sub x (CmpLT Arg n)
    | n <= 0 -> x
  Add x (MulLo (CmpLT Arg n) _)
    | n <= 0 -> x
  e -> e

-- | Reference (but slow) interpreter of 'AST'.
-- It is not meant to be used in production
-- and is provided primarily for testing purposes.
--
-- >>> interpretAST (astQuot (10 :: Data.Word.Word8)) 123
-- 12
--
interpretAST :: (Integral a, FiniteBits a) => AST a -> (a -> a)
interpretAST ast n = go ast
  where
    go = \case
      Arg       -> n
      MulHi x k -> defaultMulHi (go x) k
      MulLo x k -> go x * k
      Add x y   -> go x + go y
      Sub x y   -> go x - go y
      Shl x k   -> go x `shiftL` k
      Shr x k   -> go x `shiftR` k
      CmpGE x k -> if go x >= k then 1 else 0
      CmpLT x k -> if go x <  k then 1 else 0

defaultMulHi :: (Integral a, FiniteBits a) => a -> a -> a
defaultMulHi x y = fromInteger $ (toInteger x * toInteger y) `shiftR` finiteBitSize x

-- | Embed 'AST' into Haskell expression.
quoteAST ::
  _ => AST a -> _ (a -> a)
quoteAST = \case
  Arg            -> [|| id ||]
  Shr x k        -> [|| (`shiftR` k) . $$(quoteAST x) ||]
  Shl x k        -> [|| (`shiftL` k) . $$(quoteAST x) ||]
  MulHi x k      -> [|| (`mulHi` k) . $$(quoteAST x) ||]
  MulLo x k      -> [|| (* k) . $$(quoteAST x) ||]
  Add x y        -> [|| \w -> $$(quoteAST x) w + $$(quoteAST y) w ||]
  Sub x y        -> [|| \w -> $$(quoteAST x) w - $$(quoteAST y) w ||]
  CmpGE x k      -> [|| (\w -> fromIntegral (I# (dataToTag# (w >= k)))) . $$(quoteAST x) ||]
  CmpLT x k      -> [|| (\w -> fromIntegral (I# (dataToTag# (w <  k)))) . $$(quoteAST x) ||]

-- | 'astQuot' @d@ constructs an 'AST' representing
-- a function, equivalent to 'quot' @a@ for positive @a@,
-- but avoiding division instructions.
--
-- >>> astQuot (10 :: Data.Word.Word8)
-- Shr (MulHi Arg 205) 3
--
-- And indeed to divide 'Data.Word.Word8' by 10
-- one can multiply it by 205, take the high byte and
-- shift it right by 3. Somewhat counterintuitively,
-- this sequence of operations is faster than a single
-- division on most modern achitectures.
--
-- 'astQuot' function is polymorphic and supports both signed
-- and unsigned operands of arbitrary finite bitness.
-- Implementation is based on
-- Ch. 10 of Hacker's Delight by Henry S. Warren, 2012.
--
astQuot :: (Integral a, FiniteBits a) => a -> AST a
astQuot k
  | isSigned k = signedQuot k
  | otherwise  = unsignedQuot k

unsignedQuot :: (Integral a, FiniteBits a) => a -> AST a
unsignedQuot k'
  | isSigned k
  = error "unsignedQuot works for unsigned types only"
  | k' == 0
  = error "divisor must be positive"
  | k' == 1
  = Arg
  | k == 1
  = shr Arg kZeros
  | k' >= 1 `shiftL` (fbs - 1)
  = CmpGE Arg k'
  -- Hacker's Delight, 10-8, Listing 1
  | k >= 1 `shiftL` shft
  = shr (MulHi Arg magic) (shft + kZeros)
  -- Hacker's Delight, 10-8, Listing 3
  | otherwise
  = shr (Add (shr (Sub Arg (MulHi Arg magic)) 1) (MulHi Arg magic)) (shft - 1 + kZeros)
  where
    fbs = finiteBitSize k'
    kZeros = countTrailingZeros k'
    k = k' `shiftR` kZeros
    r0 = fromInteger ((1 `shiftL` fbs) `rem` toInteger k)
    shft = go r0 0
    magic = fromInteger ((1 `shiftL` (fbs + shft)) `quot` toInteger k + 1)

    go r s
      | (k - r) < 1 `shiftL` s = s
      | otherwise = go (r `shiftL` 1 `rem` k) (s + 1)

signedQuot :: (Integral a, FiniteBits a) => a -> AST a
signedQuot k'
  | not (isSigned k)
  = error "signedQuot works for signed types only"
  | k' <= 0
  = error "divisor must be positive"
  | k' == 1
  = Arg
  -- Hacker's Delight, 10-1, Listing 2
  | k == 1
  = shr (Add Arg (MulLo (CmpLT Arg 0) (k' - 1))) kZeros
  | k' >= 1 `shiftL` (fbs - 2)
  = Sub (CmpGE Arg k') (CmpLT Arg (1 - k'))
  -- Hacker's Delight, 10-3, Listing 2
  | magic >= 0
  = Add (shr (MulHi Arg magic) (shft + kZeros)) (CmpLT Arg 0)
  -- Hacker's Delight, 10-3, Listing 3
  | otherwise
  = Add (shr (Add Arg (MulHi Arg magic)) (shft + kZeros)) (CmpLT Arg 0)
  where
    fbs = finiteBitSize k'
    kZeros = countTrailingZeros k'
    k = k' `shiftR` kZeros
    r0 = fromInteger ((1 `shiftL` fbs) `rem` toInteger k)
    shft = go r0 0
    magic = fromInteger ((1 `shiftL` (fbs + shft)) `quot` toInteger k + 1)

    go r s
      | (k - r) < 1 `shiftL` (s + 1) = s
      | otherwise = go (r `shiftL` 1 `rem` k) (s + 1)

shr :: AST a -> Int -> AST a
shr x 0 = x
shr x k = Shr x k
