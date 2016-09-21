{-|
Module      : Idris.Package.Parser
Description : `iPKG` file parser and package description information.
Copyright   :
License     : BSD3
Maintainer  : The Idris Community.
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
#if !(MIN_VERSION_base(4,8,0))
{-# LANGUAGE OverlappingInstances #-}
#endif

module Idris.Package.Parser where

import Idris.Prelude
import Idris.CmdOptions
import Idris.Package.Common (PkgDesc(..), defaultPkg)
import Idris.Parser.Helpers hiding (stringLiteral)

import Control.Monad.State.Strict
import Control.Applicative
import System.FilePath (takeFileName, isValid)
import Data.List (union)
import Text.Trifecta hiding (span, charLiteral, natural, symbol, char, string, whiteSpace, err)
import qualified Text.PrettyPrint.ANSI.Leijen as PP


type PParser = StateT PkgDesc IdrisInnerParser

instance HasLastTokenSpan PParser where
  getLastTokenSpan = return Nothing

#if MIN_VERSION_base(4,9,0)
instance {-# OVERLAPPING #-} DeltaParsing PParser where
  line = lift line
  {-# INLINE line #-}
  position = lift position
  {-# INLINE position #-}
  slicedWith f (StateT m) = StateT $ \s -> slicedWith (\(a,s') b -> (f a b, s')) $ m s
  {-# INLINE slicedWith #-}
  rend = lift rend
  {-# INLINE rend #-}
  restOfLine = lift restOfLine
  {-# INLINE restOfLine #-}
#endif

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPING #-} TokenParsing PParser where
#else
instance TokenParsing PParser where
#endif
  someSpace = many (simpleWhiteSpace <|> singleLineComment <|> multiLineComment) *> pure ()


parseDesc :: FilePath -> IO PkgDesc
parseDesc fp = do
    p <- readFile fp
    case runparser pPkg defaultPkg fp p of
      Failure (ErrInfo err _) -> fail (show $ PP.plain err)
      Success x -> return x

pPkg :: PParser PkgDesc
pPkg = do
    reserved "package"
    p <- filename
    s <- get
    put (s { pkgname = p })
    skipSome pClause
    st <- get
    eof
    return st

-- | Parses a filename.
-- |
-- | Treated for now as an identifier or a double-quoted string.
filename :: (MonadicParsing m) => m String
filename = (do
    filePath <- token $
        -- Treat a double-quoted string as a filename to support spaces.
        -- This also moves away from tying filenames to identifiers, so
        -- it will also accept hyphens
        -- (https://github.com/idris-lang/Idris-dev/issues/2721)
        stringLiteral
        <|>
        -- Through at least version 0.9.19.1, IPKG executable values were
        -- possibly namespaced identifiers, like foo.bar.baz.
        show <$> fst <$> iName []
    case filenameErrorMessage filePath of
      Just errorMessage -> fail errorMessage
      Nothing -> return filePath)
    <?> "filename"
    where
        -- TODO: Report failing span better! We could lookAhead,
        -- or do something with DeltaParsing?
        filenameErrorMessage :: FilePath -> Maybe String
        filenameErrorMessage filePath = either Just (const Nothing) $ do
            checkEmpty filePath
            checkValid filePath
            checkNoDirectoryComponent filePath
            where
                checkThat ok message =
                    if ok then Right () else Left message

                checkEmpty path =
                    checkThat (path /= "") "filename must not be empty"

                checkValid path =
                    checkThat (System.FilePath.isValid path)
                        "filename must contain only valid characters"

                checkNoDirectoryComponent path =
                    checkThat (path == takeFileName path)
                        "filename must contain no directory component"

skipChar :: Char -> PParser ()
skipChar c = lchar c >> return ()

pClause :: PParser ()
pClause = do reserved "executable"; skipChar '='
             exec <- filename
             st <- get
             put (st { execout = Just exec })

      <|> do reserved "main"; skipChar '=';
             main <- fst <$> iName []
             st <- get
             put (st { idris_main = Just main })

      <|> do reserved "sourcedir"; skipChar '='
             src <- fst <$> identifier
             st <- get
             put (st { sourcedir = src })

      <|> do reserved "opts"; skipChar '='
             opts <- stringLiteral
             st <- get
             let args = pureArgParser (words opts)
             put (st { idris_opts = args ++ idris_opts st })

      <|> do reserved "pkgs"; skipChar '='
             ps <- sepBy1 (fst <$> identifier) (lchar ',')
             st <- get
             let pkgs = pureArgParser $ concatMap (\x -> ["-p", x]) ps

             put (st { pkgdeps    = ps `union` (pkgdeps st)
                     , idris_opts = pkgs ++ idris_opts st})

      <|> do reserved "modules"; skipChar '='
             ms <- sepBy1 (fst <$> iName []) (lchar ',')
             st <- get
             put (st { modules = modules st ++ ms })

      <|> do reserved "libs"; skipChar '='
             ls <- sepBy1 (fst <$> identifier) (lchar ',')
             st <- get
             put (st { libdeps = libdeps st ++ ls })

      <|> do reserved "objs"; skipChar '='
             ls <- sepBy1 (fst <$> identifier) (lchar ',')
             st <- get
             put (st { objs = objs st ++ ls })

      <|> do reserved "makefile"; skipChar '='
             mk <- fst <$> iName []
             st <- get
             put (st { makefile = Just (show mk) })

      <|> do reserved "tests"; skipChar '='
             ts <- sepBy1 (fst <$> iName []) (lchar ',')
             st <- get
             put st { idris_tests = idris_tests st ++ ts }

      <|> do reserved "version"
             _ <- lchar '='
             vStr <- many (satisfy (not . isEol))
             eol
             someSpace
             st <- get
             put st {pkgversion = Just vStr}

      <|> do reserved "readme"
             _ <- lchar '='
             rme <- many (satisfy (not . isEol))
             eol
             someSpace
             st <- get
             put (st { pkgreadme = Just rme })

      <|> do reserved "license"
             _ <- lchar '='
             lStr <- many (satisfy (not . isEol))
             eol
             st <- get
             put st {pkglicense = Just lStr}

      <|> do reserved "homepage"
             _ <- lchar '='
             www <- many (satisfy (not . isEol))
             eol
             someSpace
             st <- get
             put st {pkghomepage = Just www}

      <|> do reserved "sourceloc"
             _ <- lchar '='
             srcpage <- many (satisfy (not . isEol))
             eol
             someSpace
             st <- get
             put st {pkgsourceloc = Just srcpage}

      <|> do reserved "bugtracker"
             _ <- lchar '='
             src <- many (satisfy (not . isEol))
             eol
             someSpace
             st <- get
             put st {pkgbugtracker = Just src}

      <|> do reserved "brief"
             _ <- lchar '='
             brief <- stringLiteral
             st <- get
             someSpace
             put st {pkgbrief = Just brief}

      <|> do reserved "author"; _ <- lchar '=';
             author <- many (satisfy (not . isEol))
             eol
             someSpace
             st <- get
             put st {pkgauthor = Just author}

      <|> do reserved "maintainer"; _ <- lchar '=';
             maintainer <- many (satisfy (not . isEol))
             eol
             someSpace
             st <- get
             put st {pkgmaintainer = Just maintainer}
