{-|
Module      : Idris.Prelude
Description : Alternative Prelude mostly for compatibility with GHC <7.10.x.
Copyright   :
License     : BSD3
Maintainer  : The Idris Community.
-}

{-# OPTIONS_GHC -Wall #-}

module Idris.Prelude
  ( P.Bool ( False, True )
  , (P.&&), (P.||), P.not, P.otherwise

  , P.Maybe ( Nothing, Just )
  , P.maybe

  , P.Either ( Left, Right )
  , P.either

  , P.Ordering ( LT, EQ, GT )
  , P.Char, P.String

  , P.fst, P.snd, P.curry, P.uncurry

  , P.Eq ( (==), (/=) )
  , P.Ord ( compare, (<), (<=), (>=), (>), max, min )
  , P.Enum ( succ, pred, toEnum, fromEnum, enumFrom, enumFromThen
           , enumFromTo, enumFromThenTo
           )
  , P.Bounded ( minBound, maxBound )

  , P.Int, P.Integer, P.Float, P.Double
  , P.Num ( (+), (-), (*), negate, abs, signum, fromInteger )
  , P.Real ( toRational )
  , P.Integral ( quot, rem, div, mod, quotRem, divMod, toInteger )
  , P.Fractional ( (/), recip, fromRational )
  , P.Floating ( pi, exp, log, sqrt, (**), logBase, sin, cos, tan
               , asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
               )
  , P.RealFrac ( properFraction, truncate, round, ceiling, floor )
  , P.RealFloat ( floatRadix, floatDigits, floatRange, decodeFloat
                , encodeFloat, exponent, significand, scaleFloat, isNaN
                , isInfinite, isDenormalized, isIEEE, isNegativeZero
                )
  , P.subtract, P.even, P.odd, P.gcd, P.lcm, (P.^), (P.^^)
  , P.fromIntegral, P.realToFrac

  , P.Monad ( (>>=), (>>), return, fail )
  , P.Functor ( fmap )
  , P.mapM, P.mapM_, P.sequence, P.sequence_, (P.=<<)

  , P.id, P.const, (P..), P.flip, (P.$), P.until
  , P.asTypeOf, P.error, P.undefined
  , P.seq, (P.$!)

  , P.map, (P.++), P.filter
  , P.head, P.last, P.tail, P.init, P.null, P.length, (P.!!)
  , P.reverse
  , P.foldr, P.foldr1
  , P.and, P.or, P.any, P.all
  , P.sum, P.product
  , P.concat, P.concatMap
  , P.maximum, P.minimum
  , P.scanl, P.scanl1, P.scanr, P.scanr1
  , P.iterate, P.repeat, P.replicate, P.cycle
  , P.take, P.drop, P.splitAt, P.takeWhile, P.dropWhile, P.span, P.break
  , P.elem, P.notElem, P.lookup
  , P.zip, P.zip3, P.zipWith, P.zipWith3, P.unzip, P.unzip3
  , P.lines, P.words, P.unlines, P.unwords

  , P.ShowS
  , P.Show ( showsPrec, showList, show )
  , P.shows
  , P.showChar, P.showString, P.showParen

  , P.ReadS
  , P.Read ( readsPrec, readList )
  , P.reads, P.readParen, P.read, P.lex

  , P.IO
  , P.putChar, P.putStr, P.putStrLn, P.print
  , P.FilePath, P.readFile, P.writeFile, P.appendFile, P.readIO, P.readLn
  , P.IOError, P.ioError, P.userError
  ) where

import qualified Prelude as P
