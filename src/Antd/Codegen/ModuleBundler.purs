module Antd.Codegen.ModuleBundler
       ( createPSModule
       ) where

import Prelude

import Antd.Codegen.Types (AntModule, Component, PSDecl(..), PSDeclName(..), PSModule, PSRecordRow, Prop, PropTyp, Typ(..))
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (uncurry)

createPSModule :: AntModule -> PSModule
createPSModule am =
  build am.primaryComponent.name $ addComponent am.primaryComponent

type State =
  { importPrelude :: Boolean
  , exports :: Array PSDeclName
  , imports :: Map String (Set PSDeclName)
  , declarations :: Array PSDecl
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

addImportsForPropTyp :: PropTyp -> State -> State
addImportsForPropTyp { required, typ } =
  addImportsForTyp typ >>>
  if not required
  then addImportType "Untagged.Union" "UndefinedOr" false
  else identity

-- TODO add others after writing tests
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

addComponent :: Component -> State -> State
addComponent { name, props } =
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
  where
    importPropTyps =
      traverseStateBuilders (addImportsForPropTyp <<< _.propTyp) props

    pcn = mkCompNames name

addComponentImports :: State -> State
addComponentImports =
  addImportType  "React.Basic" "ReactComponent" false >>>
  addImportFun   "React.Basic" "element" >>>
  addImportClass "Untagged.Coercible" "Coercible" >>>
  addImportFun   "Untagged.Coercible" "coerce"

build :: String -> (State -> State) -> PSModule
build name builderF =
  { name: "Antd." <> name
  , exports: finalState.exports
  , importPrelude: finalState.importPrelude
  , imports
  , declarations: finalState.declarations
  }

  where
    initState =
      { importPrelude: false
      , exports: mempty
      , imports: mempty
      , declarations: mempty
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
  , propTyp: p.propTyp
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
