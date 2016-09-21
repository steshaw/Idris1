{-|
Module      : Idris.IdrisDoc
Description : Generation of HTML documentation for Idris code
Copyright   :
License     : BSD3
Maintainer  : The Idris Community.
-}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Idris.IdrisDoc (generateDocs) where

import Idris.Prelude
import Idris.Core.TT (Name(..), sUN, OutputAnnotation(..),
                      TextFormatting(..), txt, str, nsroot, constIsType, toAlist)
import Idris.Core.Evaluate (ctxtAlist, lookupDefAcc,
                            Accessibility(..), isDConName, isFnName,
                            isTConName)
import Idris.Parser.Helpers (opChars)
import Idris.AbsSyntaxTree
  ( PTerm(..), PArg, PArg'(..), PDo, PDo'(..), PTactic, PTactic'(..), Fixity(..)
  , IState(tt_ctxt, idris_moduledocs, idris_infixes)
  , HowMuchDocs(..)
  , pprintPTerm, defaultPPOption
  )
import Idris.Docs
import Idris.Docstrings (nullDocstring)
import qualified Idris.Docstrings as Docstrings

import IRTS.System (getDataFileName)

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Data.Maybe

import qualified Data.ByteString.Lazy as BS2
import Data.Foldable (foldl')
import qualified Data.List as L
import qualified Data.Map as M hiding ((!))
import Data.Monoid (mempty)
import qualified Data.Set as S
import qualified Data.Text as T

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory

import Text.PrettyPrint.Annotated.Leijen (displayDecorated, renderCompact)

import Text.Blaze (toValue, contents)
import Text.Blaze.Internal (MarkupM (Empty))
import Text.Blaze.Html5 ((!), toHtml, preEscapedToHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html.Renderer.String as R
import Text.Blaze.Renderer.String (renderMarkup)

-- ---------------------------------------------------------------- [ Public ]

-- | Generates HTML documentation for a series of loaded namespaces
--   and their dependencies.
generateDocs :: IState   -- ^ IState where all necessary information is
                         --   extracted from.
             -> [Name]   -- ^ List of namespaces to generate
                         --   documentation for.
             -> FilePath -- ^ The directory to which documentation will
                         --   be written.
             -> IO (Either String ())
generateDocs ist nss' out =
  do let nss     = map toNsName nss'
     docs       <- fetchInfo ist nss
     let (c, io) = foldl' (checker docs) (0, return ()) nss
     io
     if c < length nss
        then catchIOError (createDocs ist docs out) (err . show)
        else err "No namespaces to generate documentation for"

  where checker docs st ns | M.member ns docs = st
        checker _   (c, io) ns = (c+1, do _ <- io; warnMissing ns)
        warnMissing ns =
          putStrLn $ "Warning: Ignoring empty or non-existing namespace '" ++
                     (nsName2Str ns) ++ "'"

-- ----------------------------------------------------------------- [ Types ]

-- | Either an error message or a result
type Failable = Either String

-- | Internal representation of a fully qualified namespace name
type NsName = [T.Text]

-- | All information to be documented about a single namespace member
type NsItem = (Name, Maybe Docs, Accessibility)

-- | Docstrings containing fully elaborated term annotations
type FullDocstring = Docstrings.Docstring Docstrings.DocTerm

-- | All information to be documented about a namespace
data NsInfo = NsInfo { nsDocstring :: Maybe FullDocstring,
                       nsContents :: [NsItem]
                     }

-- | A map from namespace names to information about them
type NsDict = M.Map NsName NsInfo

-- --------------------------------------------------------------- [ Utility ]

-- | Make an error message
err :: String -> IO (Failable ())
err s = return $ Left s

-- | IdrisDoc version
version :: String
version = "1.0"

-- | Converts a Name into a [Text] corresponding to the namespace
--   part of a NS Name.
toNsName :: Name -- ^ Name to convert
         -> NsName
toNsName (UN n)    = [n]
toNsName (NS n ns) = (toNsName n) ++ ns
toNsName _         = []


-- | Retrieves the namespace part of a Name
getNs :: Name -- ^ Name to retrieve namespace for
      -> NsName
getNs (NS _ ns) = ns
getNs _         = []


-- | String to replace for the root namespace
rootNsStr :: String
rootNsStr = "[builtins]"


-- | Converts a NsName to string form
nsName2Str :: NsName -- ^ NsName to convert
           -> String
nsName2Str []  = rootNsStr
nsName2Str nsn = name nsn
  where name = str . T.intercalate "." . reverse

-- --------------------------------------------------------- [ Info Fetching ]

-- | Fetch info about namespaces and their contents
fetchInfo :: IState    -- ^ IState to fetch info from
          -> [NsName]  -- ^ List of namespaces to fetch info for
          -> IO NsDict -- ^ Mapping from namespace name to
                       --   info about its contents
fetchInfo ist nss =
  do let originNss  = S.fromList nss
     info          <- nsDict ist
     let accessible = M.map (filterContents filterInclude) info
         nonOrphan  = M.map (updateContents removeOrphans) accessible
         nonEmpty   = M.filter (not . null . nsContents) nonOrphan
         reachedNss = traceNss nonEmpty originNss S.empty
     return $ M.filterWithKey (\k _ -> S.member k reachedNss) nonEmpty
  where
    -- TODO: lensify
    filterContents p (NsInfo md ns) = NsInfo md (filter p ns)
    updateContents f x = x { nsContents = f (nsContents x) }

-- | Removes loose interface methods and data constructors,
--   leaving them documented only under their parent.
removeOrphans :: [NsItem] -- ^ List to remove orphans from
              -> [NsItem] -- ^ Orphan-free list
removeOrphans list =
  let children = S.fromList $ concatMap (names . (\(_, d, _) -> d)) list
  in  filter ((flip S.notMember children) . (\(n, _, _) -> n)) list

  where names (Just (DataDoc _ fds))                  = map (\(FD n _ _ _ _) -> n) fds
        names (Just (InterfaceDoc _ _ fds _ _ _ _ c)) = map (\(FD n _ _ _ _) -> n) fds ++ map (\(FD n _ _ _ _) -> n) (maybeToList c)
        names _                                       = []

-- | Whether a Name names something which should be documented
filterName :: Name -- ^ Name to check
           -> Bool -- ^ Predicate result
filterName (UN _)     = True
filterName (NS n _)   = filterName n
filterName _          = False


-- | Whether a NsItem should be included in the documentation.
--   It must not be Hidden/Private and filterName must return True for the name.
--   Also it must have Docs -- without Docs, nothing can be done.
filterInclude :: NsItem -- ^ Accessibility to check
              -> Bool   -- ^ Predicate result
filterInclude (name, Just _, Public) | filterName name = True
filterInclude (name, Just _, Frozen) | filterName name = True
filterInclude _                                        = False


-- | Finds all namespaces indirectly referred by a set of namespaces.
--   The NsItems of the namespaces are searched for references.
traceNss :: NsDict       -- ^ Mappings of namespaces and their contents
         -> S.Set NsName -- ^ Set of namespaces to trace
         -> S.Set NsName -- ^ Set of namespaces which has been traced
         -> S.Set NsName -- ^ Set of namespaces to trace and all traced one
traceNss nsd sT sD =
  let nsTracer ns | Just nsis <- M.lookup ns nsd = map referredNss (nsContents nsis)
      nsTracer _                                 = [S.empty] -- Ignore
      reached     = S.unions $ concatMap nsTracer (S.toList sT)
      processed   = S.union sT sD
      untraced    = S.difference reached processed
  in  if S.null untraced then processed
      else traceNss nsd untraced processed


-- | Gets all namespaces directly referred by a NsItem
referredNss :: NsItem -- ^ The name to get all directly
                      --   referred namespaces for
            -> S.Set NsName
referredNss (_, Nothing, _) = S.empty
referredNss (_, Just d, _) =
  let fds    = getFunDocs d
      ts     = concatMap types fds
      names  = concatMap (extractPTermNames) ts
  in  S.map getNs $ S.fromList names

  where getFunDocs (FunDoc f)                      = [f]
        getFunDocs (DataDoc f fs)                  = f:fs
        getFunDocs (InterfaceDoc _ _ fs _ _ _ _ _) = fs
        getFunDocs (RecordDoc _ _ f fs _)          = f:fs
        getFunDocs (NamedImplementationDoc _ fd)         = [fd]
        getFunDocs (ModDoc _ _)                    = []
        types (FD _ _ args t _)                    = t:(map second args)
        second (_, x, _, _)                        = x


-- | Returns an NsDict of containing all known namespaces and their contents
nsDict :: IState
       -> IO NsDict
nsDict ist = flip (foldl' addModDoc) modDocs $ foldl' adder (return M.empty) nameDefList
  where nameDefList    = ctxtAlist $ tt_ctxt ist
        adder m (n, _) = do map'   <- m
                            doc    <- loadDocs ist n
                            let access = getAccess ist n
                                nInfo  = NsInfo Nothing [(n, doc, access)]
                            return $ M.insertWith addNameInfo (getNs n) nInfo map'
        addNameInfo (NsInfo m ns) (NsInfo m' ns') = NsInfo (m <|> m') (ns ++ ns')
        modDocs = map (\(mn, d) -> (mn, NsInfo (Just d) [])) $ toAlist (idris_moduledocs ist)
        addModDoc :: IO NsDict -> (Name, NsInfo) -> IO NsDict
        addModDoc dict (mn, d) = fmap (M.insertWith addNameInfo (getNs mn) d) dict


-- | Gets the Accessibility for a Name
getAccess :: IState        -- ^ IState containing accessibility information
          -> Name          -- ^ The Name to retrieve access for
          -> Accessibility
getAccess ist n =
  let res = lookupDefAcc n False (tt_ctxt ist)
  in case res of
     [(_, acc)] -> acc
     _          -> Private

-- | Simple predicate for whether an NsItem has Docs
-- XXX: Unused.
-- hasDoc :: NsItem -- ^ The NsItem to test
--        -> Bool   -- ^ The result
-- hasDoc (_, Just _, _) = True
-- hasDoc _              = False


-- | Predicate saying whether a Name possibly may have docs defined
--   Without this, getDocs from Idris.Docs may fail a pattern match.
mayHaveDocs :: Name -- ^ The Name to test
            -> Bool -- ^ The result
mayHaveDocs (UN _)   = True
mayHaveDocs (NS n _) = mayHaveDocs n
mayHaveDocs _        = False


-- | Retrieves the Docs for a Name
loadDocs :: IState     -- ^ IState to extract infomation from
         -> Name       -- ^ Name to load Docs for
         -> IO (Maybe Docs)
loadDocs ist n
  | mayHaveDocs n = do docs <- runExceptT $ evalStateT (getDocs n FullDocs) ist
                       case docs of Right d -> return (Just d)
                                    Left _  -> return Nothing
  | otherwise     = return Nothing


-- | Extracts names referred from a type.
--   The covering of all PTerms ensures that we avoid unanticipated cases,
--   though all of them are not needed. The author just did not know which!
--   TODO: Remove unnecessary cases
extractPTermNames :: PTerm  -- ^ Where to extract names from
                  -> [Name] -- ^ Extracted names
extractPTermNames (PRef _ _ n)       = [n]
extractPTermNames (PInferRef _ _ n)  = [n]
extractPTermNames (PPatvar _ n)      = [n]
extractPTermNames (PLam _ n _ p1 p2) = n : concatMap extract [p1, p2]
extractPTermNames (PPi _ n _ p1 p2)  = n : concatMap extract [p1, p2]
extractPTermNames (PLet _ n _ p1 p2 p3) = n : concatMap extract [p1, p2, p3]
extractPTermNames (PTyped p1 p2)     = concatMap extract [p1, p2]
extractPTermNames (PApp _ p pas)     = let names = concatMap extractPArg pas
                                       in  (extract p) ++ names
extractPTermNames (PAppBind _ p pas) = let names = concatMap extractPArg pas
                                       in  (extract p) ++ names
extractPTermNames (PMatchApp _ n)    = [n]
extractPTermNames (PCase _ p ps)     = let (ps1, ps2) = unzip ps
                                       in  concatMap extract (p:(ps1 ++ ps2))
extractPTermNames (PIfThenElse _ c t f) = concatMap extract [c, t, f]
extractPTermNames (PRewrite _ _ a b m) | Just c <- m =
                                       concatMap extract [a, b, c]
extractPTermNames (PRewrite _ _ a b _) = concatMap extract [a, b]
extractPTermNames (PPair _ _ _ p1 p2)  = concatMap extract [p1, p2]
extractPTermNames (PDPair _ _ _ a b c) = concatMap extract [a, b, c]
extractPTermNames (PAlternative _ _ l) = concatMap extract l
extractPTermNames (PHidden p)        = extract p
extractPTermNames (PGoal _ p1 n p2)  = n : concatMap extract [p1, p2]
extractPTermNames (PDoBlock pdos)    = concatMap extractPDo pdos
extractPTermNames (PIdiom _ p)       = extract p
extractPTermNames (PMetavar _ n)     = [n]
extractPTermNames (PProof tacts)     = concatMap extractPTactic tacts
extractPTermNames (PTactics tacts)   = concatMap extractPTactic tacts
extractPTermNames (PCoerced p)       = extract p
extractPTermNames (PDisamb _ p)      = extract p
extractPTermNames (PUnifyLog p)      = extract p
extractPTermNames (PNoImplicits p)   = extract p
extractPTermNames (PRunElab _ p _)   = extract p
extractPTermNames (PConstSugar _ tm) = extract tm
extractPTermNames _                  = []

-- | Shorter name for extractPTermNames
extract :: PTerm  -- ^ Where to extract names from
        -> [Name] -- ^ Extracted names
extract                               = extractPTermNames

-- | Helper function for extractPTermNames
extractPArg :: PArg -> [Name]
extractPArg (PImp {pname=n, getTm=p}) = n : extract p
extractPArg (PExp {getTm=p})          = extract p
extractPArg (PConstraint {getTm=p})   = extract p
extractPArg (PTacImplicit {pname=n, getScript=p1, getTm=p2})
                                      = n : (concatMap extract [p1, p2])

-- | Helper function for extractPTermNames
extractPDo :: PDo -> [Name]
extractPDo (DoExp   _ p)         = extract p
extractPDo (DoBind  _ n _ p)     = n : extract p
extractPDo (DoBindP _ p1 p2 ps)  = let (ps1, ps2) = unzip ps
                                       ps'        = ps1 ++ ps2
                                   in  concatMap extract (p1 : p2 : ps')
extractPDo (DoLet   _ n _ p1 p2) = n : concatMap extract [p1, p2]
extractPDo (DoLetP  _ p1 p2)     = concatMap extract [p1, p2]

-- | Helper function for extractPTermNames
extractPTactic :: PTactic -> [Name]
extractPTactic (Intro ns)         = ns
extractPTactic (Focus n)          = [n]
extractPTactic (Refine n _)       = [n]
extractPTactic (Rewrite p)        = extract p
extractPTactic (Induction p)      = extract p
extractPTactic (CaseTac p)        = extract p
extractPTactic (Equiv p)          = extract p
extractPTactic (MatchRefine n)    = [n]
extractPTactic (LetTac n p)       = n : extract p
extractPTactic (LetTacTy n p1 p2) = n : concatMap extract [p1, p2]
extractPTactic (Exact p)          = extract p
extractPTactic (ProofSearch _ _ _ m _ ns) | Just n <- m = n : ns
extractPTactic (ProofSearch _ _ _ _ _ ns) = ns
extractPTactic (Try t1 t2)        = concatMap extractPTactic [t1, t2]
extractPTactic (TSeq t1 t2)       = concatMap extractPTactic [t1, t2]
extractPTactic (ApplyTactic p)    = extract p
extractPTactic (ByReflection p)   = extract p
extractPTactic (Reflect p)        = extract p
extractPTactic (Fill p)           = extract p
extractPTactic (GoalType _ t)     = extractPTactic t
extractPTactic (TCheck p)         = extract p
extractPTactic (TEval p)          = extract p
extractPTactic _                  = []

-- ------------------------------------------------------- [ HTML Generation ]

-- | Generates the actual HTML output based on info from a NsDict
--   A merge of the new docs and any existing docs located in the output dir
--   is attempted.
--   TODO: Ensure the merge always succeeds.
--         Currently the content of 'docs/<builtins>.html' may change between
--         runs, thus not always containing all items referred from other
--         namespace .html files.
createDocs :: IState -- ^ Needed to determine the types of names
           -> NsDict   -- ^ All info from which to generate docs
           -> FilePath -- ^ The base directory to which
                       --   documentation will be written.
           -> IO (Failable ())
createDocs ist nsd out =
  do new                <- not `fmap` (doesFileExist $ out </> "IdrisDoc")
     existing_nss       <- existingNamespaces out
     let nss             = S.union (M.keysSet nsd) existing_nss
     dExists            <- doesDirectoryExist out
     if new && dExists then err $ "Output directory (" ++ out ++ ") is" ++
                                  " already in use for other than IdrisDoc."
       else do
         createDirectoryIfMissing True out
         foldl' docGen (return ()) (M.toList nsd)
         createIndex nss out
         -- Create an empty IdrisDoc file to signal 'out' is used for IdrisDoc
         if new -- But only if it not already existed...
            then withFile (out </> "IdrisDoc") WriteMode ((flip hPutStr) "")
            else return ()
         copyDependencies out
         return $ Right ()

  where docGen io (n, c) = do _ <- io; createNsDoc ist n c out


-- | (Over)writes the 'index.html' file in the given directory with
--   an (updated) index of namespaces in the documentation
createIndex :: S.Set NsName -- ^ Set of namespace names to
                            --   include in the index
            -> FilePath     -- ^ The base directory to which
                            --   documentation will be written.
            -> IO ()
createIndex nss out =
  do (tempPath, h) <- openTempFile out "index.html"
     BS2.hPut h $ renderHtml $ wrapper Nothing $ do
       H.h1 "Namespaces"
       H.ul ! A.class_ "names" $ do
         let path ns  = "docs" ++ "/" ++ genRelNsPath ns "html"
             item ns  = do let n    = toHtml $ nsName2Str ns
                               link = toValue $ path ns
                           H.li $ H.a ! A.href link ! A.class_ "code" $ n
             sort     = L.sortBy (\n1 n2 -> reverse n1 `compare` reverse n2)
         forM_ (sort $ S.toList nss) item
     hClose h
     renameFile tempPath (out </> "index.html")


-- | Generates a HTML file for a namespace and its contents.
--   The location for e.g. Prelude.Algebra is <base>/Prelude/Algebra.html
createNsDoc :: IState   -- ^ Needed to determine the types of names
            -> NsName   -- ^ The name of the namespace to
                        --   create documentation for
            -> NsInfo   -- ^ The contents of the namespace
            -> FilePath -- ^ The base directory to which
                        --   documentation will be written.
            -> IO ()
createNsDoc ist ns content out =
  do let tpath                   = out </> "docs" </> (genRelNsPath ns "html")
         dir                     = takeDirectory tpath
         file                    = takeFileName tpath
         haveDocs (_, Just d, _) = [d]
         haveDocs _              = []
                                 -- We cannot do anything without a Doc
         content'                = concatMap haveDocs (nsContents content)
     createDirectoryIfMissing True dir
     (path, h) <- openTempFile dir file
     BS2.hPut h $ renderHtml $ wrapper (Just ns) $ do
       H.h1 $ toHtml (nsName2Str ns)
       case nsDocstring content of
         Nothing -> mempty
         Just docstring -> Docstrings.renderHtml docstring
       H.dl ! A.class_ "decls" $ forM_ content' (createOtherDoc ist)
     hClose h
     renameFile path tpath


-- | Generates a relative filepath for a namespace, appending an extension
genRelNsPath :: NsName   -- ^ Namespace to generate a path for
             -> String   -- ^ Extension suffix
             -> FilePath
genRelNsPath ns suffix = nsName2Str ns <.> suffix


-- | Generates a HTML type signature with proper tags
--   TODO: Turn docstrings into title attributes more robustly
genTypeHeader :: IState -- ^ Needed to determine the types of names
              -> FunDoc -- ^ Type to generate type declaration for
              -> H.Html -- ^ Resulting HTML
genTypeHeader ist (FD nm _ args ftype _) = do
  H.span ! A.class_ (toValue $ "name " ++ getType nm)
         ! A.title  (toValue $ show nm)
         $ toHtml $ name $ nsroot nm
  H.span ! A.class_ "word"     $ do nbsp; _ <- ":"; nbsp
  H.span ! A.class_ "signature" $ preEscapedToHtml htmlSignature

  where
        htmlSignature  = displayDecorated decorator $ renderCompact signature
        signature      = pprintPTerm defaultPPOption [] names (idris_infixes ist) ftype
        names          = [ n | (n@(UN n'), _, _, _) <- args,
                           not (T.isPrefixOf (txt "__") n') ]

        decorator (AnnConst c) s | constIsType c = htmlSpan s "type" s
                                 | otherwise     = htmlSpan s "data" s
        decorator (AnnData _ _) s                = htmlSpan s "data"    s
        decorator (AnnType _ _)   s              = htmlSpan s "type"    s
        decorator AnnKeyword    s                = htmlSpan ""  "keyword" s
        decorator (AnnBoundName n i) s | Just t <- M.lookup n docs =
          let cs = (if i then "implicit " else "") ++ "documented boundvar"
          in  htmlSpan t cs s
        decorator (AnnBoundName _ i) s =
          let cs = (if i then "implicit " else "") ++ "boundvar"
          in  htmlSpan "" cs s
        decorator (AnnName n _ _ _) s
          | filterName n = htmlLink (show n) (getType n) (link n) s
          | otherwise    = htmlSpan ""       (getType n)          s
        decorator (AnnTextFmt BoldText)      s = "<b>" ++ s ++ "</b>"
        decorator (AnnTextFmt UnderlineText) s = "<u>" ++ s ++ "</u>"
        decorator (AnnTextFmt ItalicText)    s = "<i>" ++ s ++ "</i>"
        decorator _ s = s

        htmlSpan :: String -> String -> String -> String
        htmlSpan t cs s = do
          R.renderHtml $ H.span ! A.class_ (toValue cs)
                                ! A.title (toValue t)
                                $ toHtml s
        htmlLink :: String -> String -> String -> String -> String
        htmlLink t cs a s = do
          R.renderHtml $ H.a ! A.class_ (toValue cs)
                       ! A.title (toValue t) ! A.href (toValue a)
                       $ toHtml s

        docs           = M.fromList $ mapMaybe docExtractor args
        docExtractor (_, _, _, Nothing) = Nothing
        docExtractor (n, _, _, Just d)  = Just (n, doc2Str d)
                         -- TODO: Remove <p> tags more robustly
        doc2Str d      = let dirty = renderMarkup $ contents $ Docstrings.renderHtml d
                         in  take (length dirty - 8) $ drop 3 dirty

        name (NS n ns) = show (NS (sUN $ name n) ns)
        name n         = let n' = show n
                         in  if (head n') `elem` opChars
                                then '(':(n' ++ ")")
                                else n'

        link n         = let path = genRelNsPath (getNs n) "html"
                         in  path ++ "#" ++ (show n)

        getType :: Name -> String
        getType n      = let ctxt = tt_ctxt ist
                         in  case () of
                               _ | isDConName n ctxt -> "constructor"
                               _ | isFnName   n ctxt -> "function"
                               _ | isTConName n ctxt -> "type"
                               _ | otherwise         -> ""

-- | Generates HTML documentation for a function.
createFunDoc :: IState -- ^ Needed to determine the types of names
             -> FunDoc -- ^ Function to generate block for
             -> H.Html -- ^ Resulting HTML
createFunDoc ist fd@(FD name docstring args _ fixity) = do
  H.dt ! (A.id $ toValue $ show name) $ genTypeHeader ist fd
  H.dd $ do
    (if nullDocstring docstring then Empty else Docstrings.renderHtml docstring)
    let args'             = filter (\(_, _, _, d) -> isJust d) args
    if (not $ null args') || (isJust fixity)
       then H.dl $ do
         if (isJust fixity) then do
             H.dt ! A.class_ "fixity" $ "Fixity"
             let f = fromJust fixity
             H.dd ! A.class_ "fixity" ! A.title (toValue $ show f) $ genFix f
           else Empty
         forM_ args' genArg
       else Empty

  where genFix (Infixl {prec=p})  =
          toHtml $ "Left associative, precedence " ++ show p
        genFix (Infixr {prec=p})  =
          toHtml $ "Left associative, precedence " ++ show p
        genFix (InfixN {prec=p})  =
          toHtml $ "Non-associative, precedence " ++ show p
        genFix (PrefixN {prec=p}) =
          toHtml $ "Prefix, precedence " ++ show p
        genArg (_, _, _, Nothing)           = Empty
        genArg (n, _, _, Just d) = do
          H.dt $ toHtml $ show n
          H.dd $ Docstrings.renderHtml d


-- | Generates HTML documentation for any Docs type
--   TODO: Generate actual signatures for interfaces
createOtherDoc :: IState -- ^ Needed to determine the types of names
               -> Docs   -- ^ Namespace item to generate HTML block for
               -> H.Html -- ^ Resulting HTML
createOtherDoc ist (FunDoc fd)                = createFunDoc ist fd

createOtherDoc ist (InterfaceDoc nm docstring fds _ _ _ _ c) = do
  H.dt ! (A.id $ toValue $ show nm) $ do
    H.span ! A.class_ "word" $ do _ <- "interface"; nbsp
    H.span ! A.class_ "name type"
           ! A.title  (toValue $ show nm)
           $ toHtml $ name $ nsroot nm
    H.span ! A.class_ "signature" $ nbsp
  H.dd $ do
    (if nullDocstring docstring then Empty else Docstrings.renderHtml docstring)
    H.dl ! A.class_ "decls" $ (forM_ (maybeToList c ++ fds) (createFunDoc ist))

  where name (NS n ns) = show (NS (sUN $ name n) ns)
        name n         = let n' = show n
                         in  if (head n') `elem` opChars
                                then '(':(n' ++ ")")
                                else n'

createOtherDoc ist (RecordDoc nm doc ctor projs params) = do
  H.dt ! (A.id $ toValue $ show nm) $ do
    H.span ! A.class_ "word" $ do _ <- "record"; nbsp
    H.span ! A.class_ "name type"
           ! A.title (toValue $ show nm)
           $ toHtml $ name $ nsroot nm
    H.span ! A.class_ "type" $ do nbsp ; prettyParameters
  H.dd $ do
    (if nullDocstring doc then Empty else Docstrings.renderHtml doc)
    if not $ null params
       then H.dl $ forM_ params genParam
       else Empty
    H.dl ! A.class_ "decls" $ createFunDoc ist ctor
    H.dl ! A.class_ "decls" $ forM_ projs (createFunDoc ist)
  where name (NS n ns) = show (NS (sUN $ name n) ns)
        name n         = let n' = show n
                         in if (head n') `elem` opChars
                               then '(':(n' ++ ")")
                               else n'

        genParam (n, _, d) = do
          H.dt $ toHtml $ show (nsroot n)
          H.dd $ maybe nbsp Docstrings.renderHtml d

        prettyParameters = toHtml $ unwords [show $ nsroot n | (n,_,_) <- params]

createOtherDoc ist (DataDoc fd@(FD n docstring args _ _) fds) = do
  H.dt ! (A.id $ toValue $ show n) $ do
    H.span ! A.class_ "word" $ do _ <- "data"; nbsp
    genTypeHeader ist fd
  H.dd $ do
    (if nullDocstring docstring then Empty else Docstrings.renderHtml docstring)
    let args' = filter (\(_, _, _, d) -> isJust d) args
    if not $ null args'
       then H.dl $ forM_ args' genArg
       else Empty
    H.dl ! A.class_ "decls" $ forM_ fds (createFunDoc ist)

  where genArg (_, _, _, Nothing)           = Empty
        genArg (name, _, _, Just d) = do
          H.dt $ toHtml $ show name
          H.dd $ Docstrings.renderHtml d

createOtherDoc ist (NamedImplementationDoc _ fd) = createFunDoc ist fd

createOtherDoc _   (ModDoc _  docstring) = do
  Docstrings.renderHtml docstring

-- | Generates everything but the actual content of the page
wrapper :: Maybe NsName -- ^ Namespace name, unless it is the index
        -> H.Html         -- ^ Inner HTML
        -> H.Html
wrapper optNS inner =
  let (index, s) = extractS optNS
      base       = if index then "" else "../"
      styles     = base ++ "styles.css" :: String
      indexPage  = base ++ "index.html" :: String
  in  H.docTypeHtml $ do
    H.head $ do
      H.title $ do
        _ <- "IdrisDoc"
        if index then " Index" else do
          _ <- ": "
          toHtml s
      H.link ! A.type_ "text/css" ! A.rel "stylesheet"
             ! A.href (toValue styles)
    H.body ! A.class_ (if index then "index" else "namespace") $ do
      H.div ! A.class_ "wrapper" $ do
        H.header $ do
          H.strong "IdrisDoc"
          if index then Empty else do
            _ <- ": "
            toHtml s
          H.nav $ H.a ! A.href (toValue indexPage) $ "Index"
        H.div ! A.class_ "container" $ inner
      H.footer $ do
        _ <- "Produced by IdrisDoc version "
        toHtml version

  where extractS (Just ns) = (False, nsName2Str ns)
        extractS _         = (True,  "")


-- | Non-break space character
nbsp :: H.Html
nbsp = preEscapedToHtml ("&nbsp;" :: String)


-- | Returns a list of namespaces already documented in a IdrisDoc directory
existingNamespaces :: FilePath -- ^ The base directory containing the
                               --   'docs' directory with existing
                               --   namespace pages
                   -> IO (S.Set NsName)
existingNamespaces out = do
  let docs     = out ++ "/" ++ "docs"
      str2Ns s | s == rootNsStr = []
      str2Ns s = reverse $ T.splitOn (T.singleton '.') (txt s)
      toNs  fp = do isFile    <- doesFileExist $ docs </> fp
                    let isHtml = ".html" == takeExtension fp
                        name   = dropExtension fp
                        ns     = str2Ns name
                    return $ if isFile && isHtml then Just ns else Nothing
  docsExists  <- doesDirectoryExist docs
  if not docsExists
     then    return S.empty
     else do files       <- getDirectoryContents docs
             namespaces  <- catMaybes `fmap` (sequence $ map toNs files)
             return $ S.fromList namespaces


-- | Copies IdrisDoc dependencies such as stylesheets to a directory
copyDependencies :: FilePath -- ^ The base directory to which
                             --   dependencies should be written
                 -> IO ()
copyDependencies dir =
  do styles <- getDataFileName $ "idrisdoc" </> "styles.css"
     copyFile styles (dir </> "styles.css")
