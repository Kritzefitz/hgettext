module Distribution.Simple.I18N.GetText.Autogen (generatePathsModule) where

generatePathsModule :: String -> String -> FilePath -> String
generatePathsModule moduleName domain dir =
  "module " ++ moduleName ++ " (messageCatalogDomain, messageCatalogDir) where\n\
  \  messageCatalogDir :: FilePath\n\
  \  messageCatalogDir = " ++ show dir ++ "\n\
  \  messageCatalogDomain :: String\n\
  \  messageCatalogDomain = " ++ show domain ++ "\n"
