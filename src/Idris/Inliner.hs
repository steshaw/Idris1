{-|
Module      : Idris.Inliner
Description : Idris' Inliner.
Copyright   :
License     : BSD3
Maintainer  : The Idris Community.
-}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}

module Idris.Inliner(inlineDef, inlineTerm) where

import Idris.Prelude
import Idris.Core.TT
import Idris.AbsSyntaxTree (IState)

inlineDef :: IState -> [([Name], Term, Term)] -> [([Name], Term, Term)]
inlineDef ist ds = map (\ (ns, lhs, rhs) -> (ns, lhs, inlineTerm ist rhs)) ds

-- | Inlining is either top level (i.e. not in a function arg) or argument level
--
-- For each application in a term:
--    * Check if the function is inlinable
--        (Dictionaries are inlinable in an argument, not otherwise)
--      - If so, try inlining without reducing its arguments
--        + If successful, then continue on the result (top level)
--        + If not, reduce the arguments (argument level) and try again
--      - If not, inline all the arguments (top level)
inlineTerm :: IState -> Term -> Term
inlineTerm _ term = inl term where
  inl orig@(P _ n _) = inlApp n [] orig
  inl orig@(App _ _ _)
      | (P _ fn _, args) <- unApply orig = inlApp fn args orig
  inl (Bind n (Let t v) sc) = Bind n (Let t (inl v)) (inl sc)
  inl (Bind n b sc) = Bind n b (inl sc)
  inl tm = tm

  inlApp _ _ orig = orig
