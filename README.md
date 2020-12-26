# quote-quot [![Hackage](http://img.shields.io/hackage/v/quote-quot.svg)](https://hackage.haskell.org/package/quote-quot) [![Stackage LTS](http://stackage.org/package/quote-quot/badge/lts)](http://stackage.org/lts/package/quote-quot) [![Stackage Nightly](http://stackage.org/package/quote-quot/badge/nightly)](http://stackage.org/nightly/package/quote-quot) [![GitHub Actions](https://github.com/Bodigrim/quote-quot/workflows/ci/badge.svg)](https://github.com/Bodigrim/quote-quot/actions?query=workflow%3Aci)

Generate routines for integer division, employing arithmetic
and bitwise operations only, which are __2.5x-3.5x faster__
than `quot`. Divisors must be known
in compile-time and be positive.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -ddump-simpl -dsuppress-all #-}
import Numeric.QuoteQuot

-- Equivalent to (`quot` 10).
quot10 :: Word -> Word
quot10 = $$(quoteQuot 10)
```

```haskell
>>> quot10 123
12
```

Here `-ddump-splices` demonstrates the chosen implementation
for division by 10:

```haskell
Splicing expression quoteQuot 10 ======>
((`shiftR` 3) . ((\ (W# w_a9N4) ->
  let !(# hi_a9N5, _ #) = (timesWord2# w_a9N4) 14757395258967641293##
  in W# hi_a9N5) . id))
```

And `-ddump-simpl` demonstrates generated Core:

```haskell
 quot10 = \ x_a5t2 ->
   case x_a5t2 of { W# w_acHY ->
   case timesWord2# w_acHY 14757395258967641293## of
   { (# hi_acIg, ds_dcIs #) ->
   W# (uncheckedShiftRL# hi_acIg 3#)
   }
   }
```

Benchmarks show that this implementation is __3.5x faster__
 than ``(`quot` 10)``:

```haskell
{-# LANGUAGE TemplateHaskell #-}
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
```

```
499999960000000
     (`quot` 10) 316 ms
499999960000000
$$(quoteQuot 10)  89 ms
```

Common wisdom is that such microoptimizations are negligible in practice,
but this is not always the case. For instance, quite surprisingly,
this trick alone
[made Unicode normalization of Hangul characters twice faster](https://github.com/composewell/unicode-transforms/pull/42)
in [`unicode-transforms`](http://hackage.haskell.org/package/unicode-transforms).
