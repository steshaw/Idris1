{-|
Module      : Idris.Prelude
Description : Alternative Prelude mostly for compatibility with GHC <7.10.x.
Copyright   :
License     : BSD3
Maintainer  : The Idris Community.
-}

{-# OPTIONS_GHC -Wall #-}

module Idris.Prelude
  ( P.Char, P.String, P.Int , P.Integer, P.Double
  , P.Eq(..), P.Show(..), P.Ord(..), P.Functor(..)
  , P.Monad(..), P.mapM, P.mapM_, P.sequence, P.sequence_, (P.=<<)
  , P.Ordering(..)
  , P.Maybe(..), P.maybe
  , P.Either(..), P.either

  , P.Num(..), P.Real(..), P.Integral(..), P.Floating(..)
  , P.RealFrac(..), P.RealFloat(..)
  , P.fromIntegral, P.realToFrac

  , P.Bool(..) , (P.&&), (P.||), P.not
  , P.Enum(..)
  , P.Read(..), P.read
  , P.error, P.undefined
  , P.fst, P.snd
  , P.otherwise

  , P.curry, P.uncurry, P.id, P.const, P.flip, (P..), (P.$), (P.$!)

  , P.map, (P.++), P.filter
  , P.head, P.last, P.tail, P.init, P.null, P.length, (P.!!)
  , P.reverse
  , P.foldr, P.foldr1
  , P.and, P.or, P.any, P.all
  , P.sum, P.product
  , P.concat, P.concatMap
  , P.scanl, P.scanl1, P.scanr, P.scanr1
  , P.iterate, P.repeat, P.replicate, P.cycle
  , P.take, P.drop, P.splitAt, P.takeWhile, P.dropWhile, P.span, P.break
  , P.elem, P.notElem, P.lookup
  , P.zip, P.zip3, P.zipWith, P.zipWith3, P.unzip, P.unzip3
  , P.lines, P.words, P.unlines, P.unwords

  , P.IO
  , P.putChar, P.putStr, P.putStrLn, P.print
  , P.FilePath , P.readFile, P.writeFile, P.appendFile
  ) where

import qualified Prelude as P
