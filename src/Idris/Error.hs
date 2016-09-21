{-|
Module      : Idris.Error
Description : Utilities to deal with error reporting.
Copyright   :
License     : BSD3
Maintainer  : The Idris Community.
-}

{-# OPTIONS_GHC -Wall #-}

module Idris.Error where

import Idris.AbsSyntax
  ( getIState
  , typeInType
  , setErrSpan
  , logLvl
  )
import Idris.AbsSyntaxTree
  ( PTerm(..), PDo'(..), PArg'(..)
  , Idris, IState(idris_constraints, tt_ctxt)
  , throwError, catchError
  )
import Idris.Delaborate

import Idris.Core.Evaluate (ctxtAlist)
import Idris.Core.TT
import Idris.Core.Constraints
import Idris.Output

import Control.Monad (when)
import System.IO.Error (isUserError, ioeGetErrorString)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Foldable as Foldable

iucheck :: Idris ()
iucheck = do tit <- typeInType
             ist <- getIState
             let cs = idris_constraints ist
             logLvl 7 $ "ALL CONSTRAINTS: " ++ show (length (S.toList cs))
             when (not tit) $
                   (tclift $ ucheck (idris_constraints ist)) `idrisCatch`
                              (\e -> do let fc = getErrSpan e
                                        setErrSpan fc
                                        iWarn fc $ pprintErr ist e)

showErr :: Err -> Idris String
showErr e = getIState >>= return . flip pshow e

report :: IOError -> String
report e
    | isUserError e = ioeGetErrorString e
    | otherwise     = show e

idrisCatch :: Idris a -> (Err -> Idris a) -> Idris a
idrisCatch = catchError

setAndReport :: Err -> Idris ()
setAndReport err = do ist <- getIState
                      case (unwrap err) of
                        At fc e -> do setErrSpan fc
                                      iWarn fc $ pprintErr ist e
                        _ -> do setErrSpan (getErrSpan err)
                                iWarn emptyFC $ pprintErr ist err
  where unwrap (ProofSearchFail e) = e -- remove bookkeeping constructor
        unwrap e = e

ifail :: String -> Idris a
ifail = throwError . Msg

ierror :: Err -> Idris a
ierror = throwError

tclift :: TC a -> Idris a
tclift (OK v) = return v
tclift (Error err@(At fc _)) = do setErrSpan fc; throwError err
tclift (Error err@(UniverseError fc _ _ _ _)) = do setErrSpan fc; throwError err
tclift (Error err) = throwError err

tctry :: TC a -> TC a -> Idris a
tctry tc1 tc2
    = case tc1 of
           OK v    -> return v
           Error _ -> tclift tc2

getErrSpan :: Err -> FC
getErrSpan (At fc _) = fc
getErrSpan (UniverseError fc _ _ _ _) = fc
getErrSpan _ = emptyFC

--------------------------------------------------------------------
-- Specific warnings not included in elaborator
--------------------------------------------------------------------
-- | Issue a warning on "with"-terms whose namespace is empty or nonexistent
warnDisamb :: IState -> PTerm -> Idris ()
warnDisamb _   (PQuote _) = return ()
warnDisamb _   (PRef _ _ _) = return ()
warnDisamb _   (PInferRef _ _ _) = return ()
warnDisamb _   (PPatvar _ _) = return ()
warnDisamb ist (PLam _ _ _ t b) = warnDisamb ist t >> warnDisamb ist b
warnDisamb ist (PPi _ _ _ t b) = warnDisamb ist t >> warnDisamb ist b
warnDisamb ist (PLet _ _ _ x t b) = warnDisamb ist x >> warnDisamb ist t >> warnDisamb ist b
warnDisamb ist (PTyped x t) = warnDisamb ist x >> warnDisamb ist t
warnDisamb ist (PApp _ t args) = warnDisamb ist t >>
                                 mapM_ (warnDisamb ist . getTm) args
warnDisamb ist (PWithApp _ t a) = warnDisamb ist t >> warnDisamb ist a
warnDisamb ist (PAppBind _ f args) = warnDisamb ist f >>
                                     mapM_ (warnDisamb ist . getTm) args
warnDisamb _   (PMatchApp _ _) = return ()
warnDisamb ist (PCase _ tm cases) = warnDisamb ist tm >>
                                    mapM_ (\(x,y)-> warnDisamb ist x >> warnDisamb ist y) cases
warnDisamb ist (PIfThenElse _ c t f) = mapM_ (warnDisamb ist) [c, t, f]
warnDisamb _   (PTrue _ _) = return ()
warnDisamb _   (PResolveTC _) = return ()
warnDisamb ist (PRewrite _ _ x y z) = warnDisamb ist x >> warnDisamb ist y >>
                                      Foldable.mapM_ (warnDisamb ist) z
warnDisamb ist (PPair _ _ _ x y) = warnDisamb ist x >> warnDisamb ist y
warnDisamb ist (PDPair _ _ _ x y z) = warnDisamb ist x >> warnDisamb ist y >> warnDisamb ist z
warnDisamb ist (PAlternative _ _ tms) = mapM_ (warnDisamb ist) tms
warnDisamb ist (PHidden tm) = warnDisamb ist tm
warnDisamb _   (PType _) = return ()
warnDisamb _   (PUniverse _ _) = return ()
warnDisamb ist (PGoal _ x _ y) = warnDisamb ist x >> warnDisamb ist y
warnDisamb _   (PConstant _ _) = return ()
warnDisamb _   Placeholder = return ()
warnDisamb ist (PDoBlock steps) = mapM_ wStep steps
  where wStep (DoExp _ x) = warnDisamb ist x
        wStep (DoBind _ _ _ x) = warnDisamb ist x
        wStep (DoBindP _ x y cs) = warnDisamb ist x >> warnDisamb ist y >>
                                   mapM_ (\(l, r) -> warnDisamb ist l >> warnDisamb ist r) cs
        wStep (DoLet _ _ _ x y) = warnDisamb ist x >> warnDisamb ist y
        wStep (DoLetP _ x y) = warnDisamb ist x >> warnDisamb ist y
warnDisamb ist (PIdiom _ x) = warnDisamb ist x
warnDisamb _   (PMetavar _ _) = return ()
warnDisamb ist (PProof tacs) = mapM_ (Foldable.mapM_ (warnDisamb ist)) tacs
warnDisamb ist (PTactics tacs) = mapM_ (Foldable.mapM_ (warnDisamb ist)) tacs
warnDisamb _   (PElabError _) = return ()
warnDisamb _   PImpossible = return ()
warnDisamb ist (PCoerced tm) = warnDisamb ist tm
warnDisamb ist (PDisamb ds tm) = warnDisamb ist tm >>
                                 mapM_ warnEmpty ds
  where warnEmpty d =
          when (not (any (isIn d . fst) (ctxtAlist (tt_ctxt ist)))) $
            ierror . Msg $
              "Nothing found in namespace \"" ++
              intercalate "." (map T.unpack . reverse $ d) ++
              "\"."
        isIn d (NS _ ns) = isPrefixOf d ns
        isIn _ _ = False

warnDisamb ist (PUnifyLog tm) = warnDisamb ist tm
warnDisamb ist (PNoImplicits tm) = warnDisamb ist tm
warnDisamb ist (PQuasiquote tm goal) = warnDisamb ist tm >>
                                       Foldable.mapM_ (warnDisamb ist) goal
warnDisamb ist (PUnquote tm) = warnDisamb ist tm
warnDisamb _   (PQuoteName _ _ _) = return ()
warnDisamb ist (PAs _ _ tm) = warnDisamb ist tm
warnDisamb ist (PAppImpl tm _) = warnDisamb ist tm
warnDisamb ist (PRunElab _ tm _) = warnDisamb ist tm
warnDisamb ist (PConstSugar _ tm) = warnDisamb ist tm
