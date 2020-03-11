module Antd.Codegen.PSPrinter
       ( printModule
       , printModuleSection
       , printImportSection
       , printDecl
       , printTyp
       ) where

import Prelude

import Antd.Codegen.Types (PSDecl(..), PSDeclName(..), PSImport, PSModule, Typ(..), PropTyp)
import Data.Array (fold, length, mapWithIndex)
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

    printRow ndx { name: rowName, propTyp, documentation } =
      docSection
      <> delim <> rowName <> " :: " <> printPropTyp propTyp
      where
        docSection = case documentation of
          Just d -> printDocumentation ndx d <> "\n"
          Nothing -> ""

        delim = case ndx, isJust documentation of
          0, false -> " "
          0, true -> "      "
          _, _    -> "    , "

    printDocumentation ndx doc =
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
  <> "\n" <> funName <> " props = element _foo (coerce props)"

printPropTyp :: PropTyp -> String
printPropTyp { typ, required } =
  if required
  then printTyp typ
  else printTypCons "UndefinedOr" [typ]

printTyp :: Typ -> String
printTyp TypInt = "Int"
printTyp TypString = "String"
printTyp TypNumber = "Number"
printTyp TypBoolean = "Boolean"
printTyp (TypRef { name }) = name
printTyp TypUnknown = "Foreign"
printTyp (TypStringLit str) = "StringLit \"" <> str <> "\""
printTyp (TypBooleanLit bool) = "BooleanLit \"" <> show bool <> "\""
printTyp TypNode = "JSX"
printTyp TypUnit = "Unit"
printTyp (TypOneOf ts) =
  Array.intercalate " |+| " $ printTyp <$> ts
printTyp (TypArray t) =
  printTypCons "Array" [t]
printTyp (TypFn { effectful, input, output }) =
  case effectful, input of
    false, [] -> "Unit -> " <> printPropTyp output
    false, [i0] -> printPropTyp i0 <> " -> " <> printPropTyp output
    false, is -> printUncurriedFn "Fn"
    true, [] -> printPropTypCons "Effect" [output]
    true, is -> printUncurriedFn "EffectFn"

  where
    printUncurriedFn consPrefix =
      printPropTypCons
        (consPrefix <> (show (length input)))
        (Array.snoc input output)

printTyp (TypRecord []) = "{}"
printTyp (TypRecord es) =
  "{ " <> entriesSection <> " }"
  where
    entriesSection =
      Array.intercalate ", "
      $ printEntrySection <$> es

    printEntrySection { key, propTyp } =
      key <> " :: " <> printPropTyp propTyp

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

printPropTypCons :: String -> Array PropTyp -> String
printPropTypCons cons args =
  printCons cons $ printPropTyp <$> args

printTypCons :: String -> Array Typ -> String
printTypCons cons args =
  printCons cons $ printTyp <$> args

-- typ

printCons :: String -> Array String -> String
printCons cons args =
  cons <> argsSection
  where
    argsSection = fold $ (\a -> " " <> printArg a) <$> args

printArg :: String -> String
printArg arg =
  if String.contains (Pattern " ") arg
  then "(" <> arg <> ")"
  else arg
