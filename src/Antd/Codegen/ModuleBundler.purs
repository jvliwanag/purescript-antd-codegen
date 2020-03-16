module Antd.Codegen.ModuleBundler
       ( createModuleBundle
       ) where

import Prelude

import Antd.Codegen.Types (AntModule, Component, JSExport, ModuleBundle, PSDecl(..), PSDeclName(..), PSRecordRow, Prop, PropTyp, Typ(..))
import Data.Array as Array
import Data.Foldable (class Foldable, fold, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (uncurry)

createModuleBundle :: AntModule -> ModuleBundle
createModuleBundle am =
  build am.primaryComponent.name
  $ addComponent true am.primaryComponent
  >>> traverseStateBuilders (addComponent false) am.subComponents

type State =
  { importPrelude :: Boolean
  , exports :: Array PSDeclName
  , imports :: Map String (Set PSDeclName)
  , declarations :: Array PSDecl
  , jsExports :: Array JSExport
  }

addImportPrelude :: State -> State
addImportPrelude = _ { importPrelude = true }

addImportType :: String -> String -> Boolean -> State -> State
addImportType mod name includeConstructors =
  addImport mod $ PSDeclNameType { name, includeConstructors }

addImportFun :: String -> String -> State -> State
addImportFun mod name =
  addImport mod $ PSDeclNameFun name

addImportClass :: String -> String -> State -> State
addImportClass mod name =
  addImport mod $ PSDeclNameClass name

addImport :: String -> PSDeclName -> State -> State
addImport mod name prev =
  prev { imports = newImports
       }
  where
    newImports = Map.alter (Just <<< updateModEntry) mod prev.imports

    updateModEntry Nothing = Set.singleton name
    updateModEntry (Just p) = Set.insert name p

addExportType :: String -> Boolean -> State -> State
addExportType name includeConstructors =
  addExport $ PSDeclNameType { name, includeConstructors }

addExportFun :: String -> State -> State
addExportFun name =
  addExport $ PSDeclNameFun name

addExport :: PSDeclName -> State -> State
addExport name prev =
  prev { exports = Array.snoc prev.exports name
       }

addJSExport :: JSExport -> State -> State
addJSExport b prev =
  prev { jsExports = Array.snoc prev.jsExports b
       }

addImportsForPropTyp :: PropTyp -> State -> State
addImportsForPropTyp { required, typ } =
  addImportsForTyp typ >>>
  if not required
  then addImportType "Untagged.Union" "UndefinedOr" false
  else identity

addImportsForTyp :: Typ -> State -> State
addImportsForTyp TypString = addImportPrelude
addImportsForTyp TypInt = addImportPrelude
addImportsForTyp TypNumber = addImportPrelude
addImportsForTyp TypBoolean = addImportPrelude
addImportsForTyp TypUnit = addImportPrelude
addImportsForTyp TypUnknown = addImportType "Foreign" "Foreign" false
addImportsForTyp (TypStringLit _) = addImportType "Literals" "StringLit" false
addImportsForTyp (TypBooleanLit _) = addImportType "Literals" "BooleanLit" false
addImportsForTyp TypNode = addImportType "React.Basic" "JSX" false
addImportsForTyp (TypOneOf options) =
  addImportType "Untagged.Union" "|+|" false >>>
  traverseStateBuilders addImportsForTyp options
addImportsForTyp (TypFn { effectful, input, output }) =
  addImportsForPropTyp output >>>
  traverseStateBuilders addImportsForPropTyp input >>>
  importUncurried
  where
    importUncurried = case effectful, input of
      false, [] -> addImportPrelude -- for Unit input
      false, [i0] -> identity
      false, is -> addImportType "Data.Function.Uncurried" ("Fn" <> show (Array.length is)) false
      true, [] -> addImportType "Effect" "Effect" false
      true, is -> addImportType "Effect.Uncurried" ("EffectFn" <> show (Array.length is)) false
addImportsForTyp (TypRecord fields) =
  traverseStateBuilders (addImportsForPropTyp <<< _.propTyp) fields
addImportsForTyp (TypArray a) =
  addImportsForTyp a
-- we currently assume that refs defined here are within the module
-- please add a test when this assumption no longer holds ;)
addImportsForTyp (TypRef _) = identity

addDecls :: Array PSDecl -> State -> State
addDecls decls prev = prev { declarations = prev.declarations <> decls }

addComponent :: Boolean -> Component -> State -> State
addComponent isPrimary { name, props } =
  addComponentImports
  >>> importPropTyps
  >>> addExport ( PSDeclNameType { name: pcn.propsName
                                 , includeConstructors: false
                                 }
                )
  >>> addExport (PSDeclNameFun pcn.funName)
  >>> addDecls
  [ PSDeclTypeRecord
      { name: pcn.propsName
      , rows: propToPSRecordRow <$> props
      }
  , PSDeclForeignRC pcn
  ]
  >>> addJSExport { name: pcn.foreignComponentName, member: jsMember }
  where
    importPropTyps =
      traverseStateBuilders (addImportsForPropTyp <<< _.propTyp) props

    pcn = mkCompNames name

    jsMember =
      if isPrimary
      then Nothing
      else Just name

addComponentImports :: State -> State
addComponentImports =
  addImportType  "React.Basic" "ReactComponent" false >>>
  addImportFun   "React.Basic" "element" >>>
  addImportClass "Untagged.Coercible" "Coercible" >>>
  addImportFun   "Untagged.Coercible" "coerce"

build :: String -> (State -> State) -> ModuleBundle
build name builderF =
  { name
  , psModule:
    { name: "Antd." <> name
    , exports: finalState.exports
    , importPrelude: finalState.importPrelude
    , imports
    , declarations: finalState.declarations
    }
  , jsBinding:
    { antSubmodule: name
    , exports: finalState.jsExports
    }
  }


  where
    initState =
      { importPrelude: false
      , exports: mempty
      , imports: mempty
      , declarations: mempty
      , jsExports: mempty
      }

    finalState = builderF initState

    -- note: Map.toUnfoldable already guarantees
    -- sorted keys
    imports = uncurry toPSImport <$> Map.toUnfoldable finalState.imports

    toPSImport mod namesSet =
      { mod
      , names: Set.toUnfoldable namesSet
      }


propToPSRecordRow :: Prop -> PSRecordRow
propToPSRecordRow p =
  { name: p.name
  , typ: printPropTyp p.propTyp
  , documentation
  }
  where
    docSections = Array.catMaybes
      [ p.docDescription
      , ("Type: " <> _) <$> p.docType
      , ("Default: " <> _) <$> p.docDefault
      ]

    documentation = case docSections of
      [] -> Nothing
      _ -> Just $ Array.intercalate "\n" docSections


traverseStateBuilders :: forall f a. Foldable f => (a -> State -> State) -> f a -> State -> State
traverseStateBuilders f as =
  foldl (\acc a -> acc >>> f a)  identity as
  -- also see: Data.Monoid.Endo

type CompNames =
  { funName :: String
  , propsName :: String
  , foreignComponentName :: String
  }

mkCompNames :: String -> CompNames
mkCompNames baseName =
  { funName: decapName
  , propsName: baseName <> "Props"
  , foreignComponentName: "_" <> decapName
  }

  where
    decapName = decapitalize baseName

decapitalize :: String -> String
decapitalize s = case String.uncons s of
  Just { head, tail } -> String.toLower (String.singleton head) <> tail
  Nothing -> s


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
        (consPrefix <> (show (Array.length input)))
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
