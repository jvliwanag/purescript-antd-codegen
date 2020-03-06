module Antd.Codegen.Printer
       ( printModule
       , printModuleSection
       , printImportSection
       ) where

import Prelude

import Antd.Codegen.Types (PSDeclName(..), PSImport, PSModule)
import Data.Array as Array
import Data.Either (fromRight)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)

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
  nameQ <> suffix
  where
    nameQ = if nameNeedsQuote name
            then "type (" <> name <> ")"
            else name

    suffix = if includeConstructors
             then "(..)"
             else ""
printDeclName (PSDeclNameClass n) =
  "class " <> n

nameNeedsQuote :: String -> Boolean
nameNeedsQuote = Regex.test symbolRE
  where
    symbolRE = unsafePartial $ fromRight $ regex "[^A-Za-z0-9]" noFlags
