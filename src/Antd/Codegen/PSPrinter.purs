module Antd.Codegen.PSPrinter
       ( printModule
       , printModuleSection
       , printImportSection
       , printDecl
       , printTypeDecl
       ) where

import Prelude

import Antd.Codegen.Types (PSDecl(..), PSDeclName(..), PSImport, PSModule, PSTypeArg(..), PSTypeDecl(..))
import Data.Array (fold, mapWithIndex)
import Data.Array as Array
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)

printModule :: PSModule -> String
printModule { name, exports, importPrelude, imports, declarations } =
  printModuleSection name exports
  <> "\n"
  <> "\n" <> printImportSection importPrelude imports
  <> (fold $ (\s -> "\n\n" <> printDecl s) <$> declarations)
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
      "import " <> mod <> " (" <> namesSection <> ")"
      where
        namesSection = Array.intercalate ", " $ printDeclName <$> names

printDecl :: PSDecl -> String
printDecl (PSDeclTypeRecord { name, rows }) =
  "type " <> name
  <> "\n  = {" <> rowsSection
  <> "\n    }"
  where
    rowsSection =
      Array.intercalate "\n" $ printRow `mapWithIndex` rows

    printRow ndx { name: rowName, typeDecl, doc } =
      docSection
      <> delim <> rowName <> " :: " <> printTypeDecl typeDecl
      where
        docSection = case doc of
          Just d -> printDoc ndx d <> "\n"
          Nothing -> ""

        delim = case ndx, isJust doc of
          0, false -> " "
          0, true -> "      "
          _, _    -> "    , "

    printDoc ndx doc =
      Array.intercalate "\n"
      $ (\docLnNdx t ->
          case ndx, docLnNdx of
            0, 0 -> " -- " <> t
            _, _ -> "      -- " <> t
        )
      `mapWithIndex` String.split (Pattern "\n") doc
printDecl ( PSDeclForeignRC { funName, foreignComponentName, propsName }) =
  "foreign import " <> foreignComponentName <> " :: ReactComponent " <> propsName
  <> "\n"
  <> "\n" <> funName <> " :: forall r. Coercible r " <> propsName <> " => r -> JSX"
  <> "\n" <> funName <> " props = element " <> foreignComponentName <> " (coerce props)"

printTypeDecl :: PSTypeDecl -> String
printTypeDecl (PSTypeDeclCons { consName, args }) =
  Array.intercalate " " $
  Array.cons consName (printTypeArg <$> args)

printTypeDecl (PSTypeDeclOp { symbol, args }) =
  Array.intercalate (" " <> symbol <> " ") (printTypeArg <$> args)

printTypeDecl (PSTypeDeclRecord { fields }) =
  "{" <> Array.intercalate ", " (printRow <$> fields) <> "}"
  where
    printRow { name, typeDecl } = name <> " :: " <> printTypeDecl typeDecl

printTypeArg :: PSTypeArg -> String
printTypeArg (PSTypeArgDecl d) =
  if needsParen
  then "(" <> p <> ")"
  else p
  where
    p = printTypeDecl d

    needsParen = case d of
      PSTypeDeclCons { args } -> Array.length args > 0
      PSTypeDeclOp { args } -> Array.length args > 1
      PSTypeDeclRecord _ -> false
printTypeArg (PSTypeArgSymbol s) =
  "\"" <> s <> "\""

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
