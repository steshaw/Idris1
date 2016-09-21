{-|
Module      : Idris.Elab.Transform
Description : Transformations for elaborate terms.
Copyright   :
License     : BSD3
Maintainer  : The Idris Community.
-}

{-# LANGUAGE PatternGuards #-}

module Idris.Elab.Transform where

import Idris.AbsSyntax
  ( getIState, updateIState, getContext, setContext
  , addImplPat, addImplBound, addTrans, addIBC
  , logElab
  )
import Idris.AbsSyntaxTree
  ( PTerm(..)
  , allNamesIn, infP, infTerm
  , ElabInfo(..)
  , Idris, IState(..)
  , showTmImpls
  , initEState
  , getInferTerm
  , IBCWrite(IBCTrans)
  )
import Idris.Error
import Idris.Output (sendHighlighting)

import Idris.Elab.Utils
import Idris.Elab.Term

import Idris.Core.TT
import Idris.Core.Elaborate hiding (Tactic(..), defer)
import Idris.Core.Typecheck

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Control.Monad.State.Strict as State


elabTransform :: ElabInfo -> FC -> Bool -> PTerm -> PTerm -> Idris (Term, Term)
elabTransform info fc safe lhs_in@(PApp _ (PRef _ _ tf) _) rhs_in
    = do ctxt <- getContext
         ist <- getIState
         let lhs = addImplPat ist lhs_in
         logElab 5 ("Transform LHS input: " ++ showTmImpls lhs)
         (ElabResult lhs' _ [] ctxt' newDecls highlights newGName, _) <-
              tclift $ elaborate (constraintNS info) ctxt (idris_datatypes ist) (idris_name ist) (sMN 0 "transLHS") infP initEState
                       (erun fc (buildTC ist info ETransLHS [] (sUN "transform")
                                   (allNamesIn lhs_in) (infTerm lhs)))
         setContext ctxt'
         processTacticDecls info newDecls
         sendHighlighting highlights
         updateIState $ \i -> i { idris_name = newGName }
         let lhs_tm = orderPats (getInferTerm lhs')
-- XXX: Unused.
--         let lhs_ty = getInferType lhs'
         let newargs = pvars ist lhs_tm

         (clhs_tm_in, clhs_ty) <- recheckC_borrowing False False [] (constraintNS info) fc id [] lhs_tm
         let clhs_tm = renamepats pnames clhs_tm_in
         logElab 3 ("Transform LHS " ++ show clhs_tm)
         logElab 3 ("Transform type " ++ show clhs_ty)

         let rhs = addImplBound ist (map fst newargs) rhs_in
         logElab 5 ("Transform RHS input: " ++ showTmImpls rhs)

         ((rhs', _, ctxt', newDecls, newGName), _) <-
              tclift $ elaborate (constraintNS info) ctxt (idris_datatypes ist) (idris_name ist) (sMN 0 "transRHS") clhs_ty initEState
                       (do pbinds ist lhs_tm
                           setNextName
                           (ElabResult _ _ _ ctxt' newDecls highlights newGName) <- erun fc (build ist info ERHS [] (sUN "transform") rhs)
                           set_global_nextname newGName
                           erun fc $ psolve lhs_tm
                           tt <- get_term
                           let (rhs', defer) = runState (collectDeferred Nothing [] ctxt tt) []
                           newGName <- get_global_nextname
                           return (rhs', defer, ctxt', newDecls, newGName))
         setContext ctxt'
         processTacticDecls info newDecls
         sendHighlighting highlights
         updateIState $ \i -> i { idris_name = newGName }

         (crhs_tm_in, crhs_ty) <- recheckC_borrowing False False [] (constraintNS info) fc id [] rhs'
         let crhs_tm = renamepats pnames crhs_tm_in
         logElab 3 ("Transform RHS " ++ show crhs_tm)

         -- Types must always convert
         case converts ctxt [] clhs_ty crhs_ty of
              OK _ -> return ()
              Error e -> ierror (At fc (CantUnify False (clhs_tm, Nothing) (crhs_tm, Nothing) e [] 0))
         -- In safe mode, values must convert (Thinks: This is probably not
         -- useful as is, perhaps it should require a proof of equality instead)
         when safe $ case converts ctxt [] clhs_tm crhs_tm of
              OK _ -> return ()
              Error e -> ierror (At fc (CantUnify False (clhs_tm, Nothing) (crhs_tm, Nothing) e [] 0))

         case unApply (depat clhs_tm) of
              (P _ tfname _, _) -> do addTrans tfname (clhs_tm, crhs_tm)
                                      addIBC (IBCTrans tf (clhs_tm, crhs_tm))
              _ -> ierror (At fc (Msg "Invalid transformation rule (must be function application)"))
         return (clhs_tm, crhs_tm)

  where
    depat (Bind n (PVar t) sc) = depat (instantiate (P Bound n t) sc)
    depat x = x

    renamepats (n' : ns) (Bind _ (PVar t) sc)
       = Bind n' (PVar t) (renamepats ns sc) -- all Vs
    renamepats _ sc = sc

    -- names for transformation variables. Need to ensure these don't clash
    -- with any other names when applying rules, so rename here.
    pnames = map (\i -> sMN i ("tvar" ++ show i)) [0..]

elabTransform _ fc _ _ _
   = ierror (At fc (Msg "Invalid transformation rule (must be function application)"))
