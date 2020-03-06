module Antd.Codegen.Printer
       ( printModule
       , printModuleSection
       , printImportSection
       ) where

import Prelude

import Antd.Codegen.Types (PSDeclName(..), PSImport, PSModule)
import Data.Array as Array

printModule :: PSModule -> String
printModule { name, exports, importPrelude, imports } =
  printModuleSection name exports
  <> "\n"
  <> "\n" <> printImportSection importPrelude imports
  <> "\n"

printModuleSection :: String -> Array PSDeclName -> String
printModuleSection name exports =
  "module " <> name
  <> "\n" <> importSection

  where
    importSection = case exports of
      [] -> "  where"
      _ ->
        "  ( "
        <> (Array.intercalate "\n  , " $ printDeclName <$> exports)
        <> "\n  ) where"

printImportSection :: Boolean -> Array PSImport -> String
printImportSection importPrelude imports =
  importPreludeSection <> otherImportsSection
  where
    importPreludeSection =
      if importPrelude
      then "import Prelude\n"
      else ""

    otherImportsSection = case imports of
      [] -> ""
      _ -> "\n" <> (Array.intercalate "\n" $ printImport <$> imports)

    printImport { mod, names } =
      "import " <> mod <> "(" <> namesSection <> ")"
      where
        namesSection = Array.intercalate ", " $ printDeclName <$> names

-- Utils

printDeclName :: PSDeclName -> String
printDeclName (PSDeclNameFun n) = n
printDeclName (PSDeclNameType { name, includeConstructors }) =
  if includeConstructors
  then name <> "(..)"
  else name
printDeclName (PSDeclNameClass n) =
  "class " <> n
