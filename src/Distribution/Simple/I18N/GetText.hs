-- | This library extends the Distribution with internationalization support.
--
-- It performs two functions:
--
-- * compiles and installs PO files to the specified directory
--
-- * tells the application where files were installed to make it able
-- to bind them to the code
--
-- Each PO file will be placed to the
-- @{datadir}\/locale\/{loc}\/LC_MESSAGES\/{domain}.mo@ where:
--
--  [@datadir@] Usually @prefix/share@ but could be different, depends
--  on system.
--
--  [@loc@] Locale name (language code, two characters). This module
--  supposes, that each PO file has a base name set to the proper
--  locale, e.g. @de.po@ is the German translation of the program, so
--  this file will be placed under @{datadir}\/locale\/de@ directory
--
--  [@domain@] Program domain. A unique identifier of single
--  translational unit (program). By default domain will be set to the
--  package name, but its name could be configured in the @.cabal@ file.
--
-- The module defines following @.cabal@ fields:
--
--  [@x-gettext-domain-name@] Name of the domain. One or more
--  alphanumeric characters separated by hyphens or underlines. When
--  not set, package name will be used.
--
--  [@x-gettext-po-files@] List of files with translations. Could be
--  used a limited form of wildcards, e.g.:
--  @x-gettext-po-files: po/*.po@
--
-- The configured domain name and the location of the installed
-- translation files can be imported from an automatically generated
-- module named Paths_Hgettext_<pkgname> where <pkgname> is the name
-- of your package with all hyphens replaced by underscores. This
-- module exports the constants @messageCatalogDomain :: String@ and
-- @messageCatalogDir :: FilePath@. You can use it like this:
--
--
-- > import Paths_Hgettext_test_package
-- >
-- > prepareI18N = do
-- >    setLocale LC_ALL (Just "")
-- >    bindTextDomain messageCatalogDomain (Just messageCatalogDir)
-- >    textDomain messageCatalogDomain
-- >
-- > main = do
-- >    prepareI18N
-- >    ...
-- >
-- > ...
--
--
-- __NOTE:__ files, passed in the @x-gettext-po-files@ are not
-- automatically added to the source distribution, so they should be
-- also added to the @extra-source-files@ parameter, along with
-- translation template file (usually @message.pot@)
--
-- __WARNING:__ sometimes, when only configuration targets changes, code
-- will not recompile, thus you should execute @cabal clean@ to
-- cleanup the build and restart it again from the configuration. This
-- is temporary bug, it will be fixed in next releases.
--

module Distribution.Simple.I18N.GetText
    ( installGetTextHooks
    , gettextDefaultMain
    ) where

import Distribution.Simple.I18N.GetText.Autogen

import           Distribution.ModuleName
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.BuildPaths
import           Distribution.Simple.InstallDirs    as I
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.Types.ForeignLib
import           Distribution.Verbosity

import           Control.Arrow                      (second)
import           Control.Monad
import           Data.List                          (unfoldr)
import           Data.Maybe                         (fromMaybe, listToMaybe)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

import           Internal

-- | Default main function, same as
--
-- > defaultMainWithHooks $ installGetTextHooks simpleUserHooks
--
gettextDefaultMain :: IO ()
gettextDefaultMain = defaultMainWithHooks $ installGetTextHooks simpleUserHooks

-- | Installs hooks, used by GetText module to install
-- PO files to the system.
--
-- Pre-existing hook handlers are executed before the GetText
-- handlers.
--
installGetTextHooks :: UserHooks -- ^ initial user hooks
                    -> UserHooks -- ^ patched user hooks
installGetTextHooks uh =
    uh { readDesc = fmap addAutogenModules <$> readDesc uh
       , buildHook = \pd lbi uh' bf -> do
           let verbosity = fromFlag $ buildVerbosity bf
           rewriteHgettextPathsModule pd lbi verbosity
           (buildHook uh) pd lbi uh' bf

       , postInst = \args iflags pd lbi -> do
           postInst uh args iflags pd lbi
           installPOFiles (fromFlagOrDefault maxBound (installVerbosity iflags)) lbi

       , postCopy = \args cflags pd lbi -> do
           postCopy uh args cflags pd lbi
           installPOFiles (fromFlagOrDefault maxBound (copyVerbosity cflags)) lbi
       }

addAutogenModules :: GenericPackageDescription -> GenericPackageDescription
addAutogenModules gpd = gpd { packageDescription = addToPackageDescription $ packageDescription gpd }
  where addToPackageDescription pd = pd { library = addToLibrary pd <$> library pd
                                        , subLibraries = addToLibrary pd <$> subLibraries pd
                                        , executables = addToExecutable pd <$> executables pd
                                        , foreignLibs = addToForeignLib pd <$> foreignLibs pd
                                        , testSuites = addToTestSuite pd <$> testSuites pd
                                        , benchmarks = addToBenchmark pd <$> benchmarks pd
                                        }
        addToLibrary pd lib = lib { libBuildInfo = addToBuildInfo pd $ libBuildInfo lib }
        addToExecutable pd exe = exe { buildInfo = addToBuildInfo pd $ buildInfo exe }
        addToForeignLib pd lib = lib { foreignLibBuildInfo = addToBuildInfo pd $
                                                          foreignLibBuildInfo lib
                                  }
        addToTestSuite pd suite = suite { testBuildInfo = addToBuildInfo pd $ testBuildInfo suite }
        addToBenchmark pd bm = bm { benchmarkBuildInfo = addToBuildInfo pd $ benchmarkBuildInfo bm }
        addToBuildInfo pd bi = bi { autogenModules = fromString (hgettextAutogenModuleName pd)
                                                     : autogenModules bi
                                  }

rewriteHgettextPathsModule :: PackageDescription -> LocalBuildInfo -> Verbosity -> IO ()
rewriteHgettextPathsModule pd lbi verbosity = do
  let pathsModuleName = hgettextAutogenModuleName pd
      autogenDir = autogenPackageModulesDir lbi
      autogenModulePath = autogenDir </> pathsModuleName <.> "hs"
      sMap = getCustomFields lbi
      dom = getDomainNameDefault sMap (getPackageName lbi)
      tar = targetDataDir lbi
  createDirectoryIfMissingVerbose verbosity True autogenDir
  rewriteFileEx verbosity autogenModulePath $ generatePathsModule pathsModuleName dom tar

installPOFiles :: Verbosity -> LocalBuildInfo -> IO ()
installPOFiles verb l =
    let sMap = getCustomFields l
        destDir = targetDataDir l
        dom = getDomainNameDefault sMap (getPackageName l)
        installFile file = do
          let fname = takeFileName file
          let bname = takeBaseName fname
          let targetDir = destDir </> bname </> "LC_MESSAGES"
          -- ensure we have directory destDir/{loc}/LC_MESSAGES
          createDirectoryIfMissing True targetDir
          ph <- runProcess "msgfmt" [ "--output-file=" ++ (targetDir </> dom <.> "mo"), file ]
                           Nothing Nothing Nothing Nothing Nothing
          ec <- waitForProcess ph
          case ec of
            ExitSuccess   -> return ()
            -- only warn for now, as the package may still be usable even if the msg catalogs are missing
            ExitFailure n -> warn verb ("'msgfmt' exited with non-zero status (rc = " ++ show n ++ ")")
    in do
      filelist <- getPoFilesDefault sMap
      -- copy all whose name is in the form of dir/{loc}.po to the
      -- destDir/{loc}/LC_MESSAGES/dom.mo
      -- with the 'msgfmt' tool
      mapM_ installFile filelist

hgettextAutogenModuleName :: PackageDescription -> String
hgettextAutogenModuleName pd = let packagePart = map fixChar $ unPackageName $ packageName pd
                               in "Paths_Hgettext_" ++ packagePart
  where fixChar '-' = '_'
        fixChar c = c

targetDataDir :: LocalBuildInfo -> FilePath
targetDataDir l =
    let dirTmpls = installDirTemplates l
        prefix' = prefix dirTmpls
        data' = datadir dirTmpls
        dataEx = I.fromPathTemplate $ I.substPathTemplate [(PrefixVar, prefix')] data'
    in dataEx ++ "/locale"

getPackageName :: LocalBuildInfo -> String
getPackageName = fromPackageName . packageName . localPkgDescr

getCustomFields :: LocalBuildInfo -> [(String, String)]
getCustomFields = customFieldsPD . localPkgDescr

findInParametersDefault :: [(String, String)] -> String -> String -> String
findInParametersDefault al name def = (fromMaybe def . lookup name) al

getDomainNameDefault :: [(String, String)] -> String -> String
getDomainNameDefault al d = findInParametersDefault al "x-gettext-domain-name" d

getPoFilesDefault :: [(String, String)] -> IO [String]
getPoFilesDefault al = toFileList $ findInParametersDefault al "x-gettext-po-files" ""
    where toFileList "" = return []
          toFileList x  = liftM concat $ mapM matchFileGlob $ split' x
          -- from Blow your mind (HaskellWiki)
          -- splits string by newline, space and comma
          split' x = concatMap lines $ concatMap words $ unfoldr (\b -> fmap (const . (second $ drop 1) . break (==',') $ b) . listToMaybe $ b) x
