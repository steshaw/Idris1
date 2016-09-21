{-|
Module      : Idris.REPL
Description : Main function to decide Idris' mode of use.
License     : BSD3
Maintainer  : The Idris Community.
-}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Idris.Main
  ( idrisMain
  , idris
  , runMain
  , runClient -- taken from Idris.REPL.
  , loadInputs -- taken from Idris.ModeCommon
  ) where

import Idris.Prelude
import Idris.AbsSyntax
  ( getIState, putIState, getContext, runIO, logLvl
  , setWidth, setSourceDirs, setOptLevel, setColourise
  , setREPL, setQuiet, setVerbose, setCmdLine, setOutputTy
  , setNoBanner, setCodegen, setIBCSubDir, setIBCSubDir, setImportDirs
  , setLogLevel, setLogCats, setTypeCase, setTypeInType, setCoverage
  , setErrContext, setIndentWith, setIndentClause
  , addLangExt, addFlag, addAutoImport, addImportDir, addIBC
  , addOptimise, removeOptimise
  , noErrors, colourise
  , opt
  , getConsoleWidth, getFile, getOutput, getIBCSubDir, getImportDir, getSourceDir
  , getPkgDir, getBC, getOptLevel, getOutputTy, getCodegen, getCodegenArgs
  , getOptimisation, getColour, getLanguageExt, getExecScript, getEvalExpr
  , getPort, getPkgIndex
  )
import Idris.AbsSyntaxTree
  ( Opt(..)
  , Optimisation , REPLPort(..), Codegen(..), IRFormat(..)
  , DefaultTotality(..), ConsoleWidth(..)
  , IBCWrite(IBCImportDir)
  , Idris
  , IState(default_total, idris_totcheckfail)
  , idrisInit
  )
import Idris.ModeCommon
import Idris.REPL.Parser
import Idris.Error
import Idris.IBC
import Idris.Parser (loadModule, parseExpr, fixColour)
import Idris.Output

import Idris.REPL.Commands
import Idris.REPL

import Idris.ElabDecls
import Idris.Elab.Value
import Idris.Elab.Term
import Idris.Info

import Idris.Core.Execute (execute)
import Idris.Core.TT hiding (str)

import IRTS.CodegenCommon

import Util.System

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (execStateT)
import Control.Monad.Trans (lift)
import Data.Maybe
import qualified System.Console.Haskeline as H
import System.FilePath
import System.Exit
import System.Directory
import System.IO
import Text.Trifecta.Result(Result(..), ErrInfo(..))

-- | How to run Idris programs.
runMain :: Idris () -> IO ()
runMain prog = do res <- runExceptT $ execStateT prog idrisInit
                  case res of
                       Left err -> putStrLn $ "Uncaught error: " ++ show err
                       Right _ -> return ()

-- | The main function of Idris that when given a set of Options will
-- launch Idris into the desired interaction mode either: REPL;
-- Compiler; Script execution; or IDE Mode.
idrisMain :: [Opt] -> Idris ()
idrisMain opts =
  do   mapM_ setWidth (opt getConsoleWidth opts)
       let inputs = opt getFile opts
       let quiet = Quiet `elem` opts
       let nobanner = NoBanner `elem` opts
       let idesl = Idemode `elem` opts || IdemodeSocket `elem` opts
       let runrepl = not (NoREPL `elem` opts)
       let verbose = runrepl || Verbose `elem` opts
       let output = opt getOutput opts
       let ibcsubdir = opt getIBCSubDir opts
       let importdirs = opt getImportDir opts
       let sourcedirs = opt getSourceDir opts
       setSourceDirs sourcedirs
       let bcs = opt getBC opts
       let pkgdirs = opt getPkgDir opts
       -- Set default optimisations
       let optimise = case opt getOptLevel opts of
                        [] -> 2
                        xs -> last xs

       setOptLevel optimise
       let outty = case opt getOutputTy opts of
                     [] -> if Interface `elem` opts then
                              Object else Executable
                     xs -> last xs
       let cgn = case opt getCodegen opts of
                   [] -> Via IBCFormat "c"
                   xs -> last xs
       let cgFlags = opt getCodegenArgs opts

       -- Now set/unset specifically chosen optimisations
       let os = opt getOptimisation opts

       mapM_ processOptimisation os

       script <- case opt getExecScript opts of
                   []     -> return Nothing
                   _:_:_  -> do iputStrLn "More than one interpreter expression found."
                                runIO $ exitWith (ExitFailure 1)
                   [expr] -> return (Just expr)
       let immediate = opt getEvalExpr opts
       let port = case getPort opts of
                    Nothing -> ListenPort defaultPort
                    Just p  -> p

       when (DefaultTotal `elem` opts) $ do i <- getIState
                                            putIState (i { default_total = DefaultCheckingTotal })
       tty <- runIO isATTY
       setColourise $ not quiet && last (tty : opt getColour opts)

       mapM_ addLangExt (opt getLanguageExt opts)
       setREPL runrepl
       setQuiet (quiet || isJust script || not (null immediate))
       setVerbose verbose
       setCmdLine opts
       setOutputTy outty
       setNoBanner nobanner
       setCodegen cgn
       mapM_ (addFlag cgn) cgFlags
       mapM_ makeOption opts
       -- if we have the --bytecode flag, drop into the bytecode assembler
       case bcs of
         [] -> return ()
         _  -> return () -- runIO $ mapM_ bcAsm xs
       case ibcsubdir of
         [] -> setIBCSubDir ""
         (d:_) -> setIBCSubDir d
       setImportDirs importdirs

       when (not (NoBasePkgs `elem` opts)) $ do
           addPkgDir "prelude"
           addPkgDir "base"
       mapM_ addPkgDir pkgdirs
       elabPrims
       when (not (NoBuiltins `elem` opts)) $ do _ <- loadModule "Builtins" (IBC_REPL True)
                                                addAutoImport "Builtins"
                                                return ()
       when (not (NoPrelude `elem` opts)) $ do _ <- loadModule "Prelude" (IBC_REPL True)
                                               addAutoImport "Prelude"
                                               return ()
       when (runrepl && not idesl) initScript

       when (runrepl &&
             not quiet &&
             not idesl &&
             not (isJust script) &&
             not nobanner &&
             null immediate) $
         iputStrLn banner

       orig <- getIState

       mods <- if idesl then return [] else loadInputs inputs Nothing
       let efile = case inputs of
                        [] -> ""
                        (f:_) -> f

       runIO $ hSetBuffering stdout LineBuffering

       ok <- noErrors
       when ok $ case output of
                    [] -> return ()
                    (o:_) -> idrisCatch (process "" (Compile cgn o))
                               (\e -> do ist <- getIState ; iputStrLn $ pshow ist e)

       case immediate of
         [] -> return ()
         exprs -> do setWidth InfinitelyWide
                     mapM_ (\str -> do ist <- getIState
                                       c <- colourise
                                       case parseExpr ist str of
                                         Failure (ErrInfo err _) -> do iputStrLn $ show (fixColour c err)
                                                                       runIO $ exitWith (ExitFailure 1)
                                         Success e -> process "" (Eval e))
                           exprs
                     runIO exitSuccess


       case script of
         Nothing -> return ()
         Just expr -> execScript expr

       -- Create Idris data dir + repl history and config dir
       idrisCatch (do dir <- runIO $ getIdrisUserDataDir
                      exists <- runIO $ doesDirectoryExist dir
                      unless exists $ logLvl 1 ("Creating " ++ dir)
                      runIO $ createDirectoryIfMissing True (dir </> "repl"))
         (\_ -> return ())

       historyFile <- runIO $ getIdrisHistoryFile

       when ok $ case opt getPkgIndex opts of
                      (f : _) -> writePkgIndex f
                      _ -> return ()

       when (runrepl && not idesl) $ do
--          clearOrigPats
         case port of
           DontListen -> return ()
           ListenPort port' -> startServer port' orig mods
         H.runInputT (replSettings (Just historyFile)) $ repl (force orig) mods efile
       let idesock = IdemodeSocket `elem` opts
       when (idesl) $ idemodeStart idesock orig inputs
       ok' <- noErrors
       when (not ok') $ runIO (exitWith (ExitFailure 1))
  where
    makeOption (OLogging i)     = setLogLevel i
    makeOption (OLogCats cs)    = setLogCats cs
    makeOption TypeCase         = setTypeCase True
    makeOption TypeInType       = setTypeInType True
    makeOption NoCoverage       = setCoverage False
    makeOption ErrContext       = setErrContext True
    makeOption (IndentWith n)   = setIndentWith n
    makeOption (IndentClause n) = setIndentClause n
    makeOption _                = return ()

    processOptimisation :: (Bool, Optimisation) -> Idris ()
    processOptimisation (True,  p) = addOptimise p
    processOptimisation (False, p) = removeOptimise p

    addPkgDir :: String -> Idris ()
    addPkgDir p = do ddir <- runIO getIdrisLibDir
                     addImportDir (ddir </> p)
                     addIBC (IBCImportDir (ddir </> p))


-- | Invoke as if from command line. It is an error if there are
-- unresolved totality problems.
idris :: [Opt] -> IO (Maybe IState)
idris opts = do res <- runExceptT $ execStateT totalMain idrisInit
                case res of
                  Left err -> do putStrLn $ pshow idrisInit err
                                 return Nothing
                  Right ist -> return (Just ist)
    where totalMain = do idrisMain opts
                         ist <- getIState
                         case idris_totcheckfail ist of
                           ((fc, msg):_) -> ierror . At fc . Msg $ "Could not build: "++  msg
                           [] -> return ()


-- | Execute the provided Idris expression.
execScript :: String -> Idris ()
execScript expr = do i <- getIState
                     c <- colourise
                     case parseExpr i expr of
                          Failure (ErrInfo err _) -> do iputStrLn $ show (fixColour c err)
                                                        runIO $ exitWith (ExitFailure 1)
                          Success term -> do _ <- getContext
                                             (tm, _) <- elabVal (recinfo (fileFC "toplevel")) ERHS term
                                             _ <- execute tm
                                             runIO $ exitSuccess

-- | Run the initialisation script
initScript :: Idris ()
initScript = do script <- runIO $ getIdrisInitScript
                idrisCatch (do go <- runIO $ doesFileExist script
                               when go $ do
                                 h <- runIO $ openFile script ReadMode
                                 runInit h
                                 runIO $ hClose h)
                           (\e -> iPrintError $ "Error reading init file: " ++ show e)
    where runInit :: Handle -> Idris ()
          runInit h = do eof <- lift . lift $ hIsEOF h
                         ist <- getIState
                         unless eof $ do
                           line <- runIO $ hGetLine h
                           script <- runIO $ getIdrisInitScript
                           c <- colourise
                           processLine ist line script c
                           runInit h
          processLine i cmd input clr =
              case parseCmd i input cmd of
                   Failure (ErrInfo err _)       -> runIO $ print (fixColour clr err)
                   Success (Right Reload)        -> iPrintError "Init scripts cannot reload the file"
                   Success (Right (Load _ _))    -> iPrintError "Init scripts cannot load files"
                   Success (Right (ModImport _)) -> iPrintError "Init scripts cannot import modules"
                   Success (Right Edit)          -> iPrintError "Init scripts cannot invoke the editor"
                   Success (Right Proofs)        -> proofs i
                   Success (Right Quit)          -> iPrintError "Init scripts cannot quit Idris"
                   Success (Right cmd')          -> process [] cmd'
                   Success (Left err)            -> runIO $ print err
