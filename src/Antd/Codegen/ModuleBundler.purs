module Antd.Codegen.ModuleBundler
       ( createModuleBundle
       ) where

import Prelude

import Antd.Codegen.Types (AntModule, Component, JSExport, ModuleBundle, PSDecl(..), PSDeclName(..), PSRecordRow, PSTypeDecl, Prop, PropTyp, Typ(..), psTypeArgSymbol, psTypeDecl, psTypeDecl', psTypeDeclOp, psTypeDeclRecord, psTypeDecl_)
import Control.Monad.State (State, execState, modify_)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (uncurry)

createModuleBundle :: AntModule -> ModuleBundle
createModuleBundle am =
  build am.primaryComponent.name
  $ addComponent true am.primaryComponent
  *> traverse_ (addComponent false) am.subComponents

type BuilderState =
  { importPrelude :: Boolean
  , exports :: Array PSDeclName
  , imports :: Map String (Set PSDeclName)
  , declarations :: Array PSDecl
  , jsExports :: Array JSExport
  }

type Builder a = State BuilderState a

addImportPrelude :: Builder Unit
addImportPrelude = modify_ _ { importPrelude = true }

addImportType :: String -> String -> Boolean -> Builder Unit
addImportType mod name includeConstructors =
  addImport mod $ PSDeclNameType { name, includeConstructors }

addImportFun :: String -> String -> Builder Unit
addImportFun mod name =
  addImport mod $ PSDeclNameFun name

addImportClass :: String -> String -> Builder Unit
addImportClass mod name =
  addImport mod $ PSDeclNameClass name

addImport :: String -> PSDeclName -> Builder Unit
addImport mod name =
  modify_ \p -> p { imports = newImports p
                        }
  where
    newImports p = Map.alter (Just <<< updateModEntry) mod p.imports

    updateModEntry Nothing = Set.singleton name
    updateModEntry (Just p) = Set.insert name p

addExportType :: String -> Boolean -> Builder Unit
addExportType name includeConstructors =
  addExport $ PSDeclNameType { name, includeConstructors }

addExportFun :: String -> Builder Unit
addExportFun name =
  addExport $ PSDeclNameFun name

addExport :: PSDeclName -> Builder Unit
addExport name =
  modify_ \p -> p { exports = Array.snoc p.exports name
                  }

addJSExport :: JSExport -> Builder Unit
addJSExport b =
  modify_ \p ->p { jsExports = Array.snoc p.jsExports b
                 }

usePropTypeDecl :: PropTyp -> Builder PSTypeDecl
usePropTypeDecl { required, typ } = do
  d <- useTypeDecl typ
  if not required
    then addImportType "Untagged.Union" "UndefinedOr" false $> psTypeDecl "UndefinedOr" [d]
    else pure d

useTypeDecl :: Typ -> Builder PSTypeDecl
useTypeDecl TypString = addImportPrelude $> psTypeDecl_ "String"
useTypeDecl TypInt = addImportPrelude $> psTypeDecl_ "Int"
useTypeDecl TypNumber = addImportPrelude $> psTypeDecl_ "Number"
useTypeDecl TypBoolean = addImportPrelude $> psTypeDecl_ "Boolean"
useTypeDecl TypUnit = addImportPrelude $> psTypeDecl_ "Unit"
useTypeDecl TypUnknown = addImportType "Foreign" "Foreign" false $> psTypeDecl_ "Foreign"
useTypeDecl (TypStringLit s) =
  addImportType "Literals" "StringLit" false
  $> (psTypeDecl' "StringLit" [psTypeArgSymbol s])
useTypeDecl (TypBooleanLit b) =
  addImportType "Literals" "BooleanLit" false
  $> (psTypeDecl' "BooleanLit" [psTypeArgSymbol $ show b])
useTypeDecl TypNode =
  addImportType "React.Basic" "JSX" false
  $> psTypeDecl_ "JSX"
useTypeDecl (TypOneOf options) = do
  addImportType "Untagged.Union" "|+|" false
  psTypeDeclOp "|+|" <$> traverse useTypeDecl options
useTypeDecl (TypFn { effectful, input, output }) = do
  fn <- importUncurried
  outArg <- usePropTypeDecl output
  inArgs <- traverse usePropTypeDecl input
  pure $ fn $ Array.snoc inArgs outArg

  where
    importUncurried :: Builder (Array PSTypeDecl -> PSTypeDecl)
    importUncurried = case effectful, input of
      false, [] -> addImportPrelude $> (Array.cons (psTypeDecl_ "Unit") >>> psTypeDeclOp "->")
      false, [i0] -> pure $ psTypeDeclOp "->"
      false, is ->
        let fnName = ("Fn" <> show (Array.length is))
        in addImportType "Data.Function.Uncurried" fnName false $> psTypeDecl fnName
      true, [] ->
        addImportType "Effect" "Effect" false $> psTypeDecl "Effect"
      true, is ->
        let fnName = ("EffectFn" <> show (Array.length is))
        in addImportType "Effect.Uncurried" fnName false $> psTypeDecl fnName
useTypeDecl (TypRecord fields) =
  psTypeDeclRecord <$> traverse addRow fields
  where
    addRow { key, propTyp } =
      usePropTypeDecl propTyp <#> { name: key, typeDecl: _ }
useTypeDecl (TypArray a) = do
  decl <- useTypeDecl a
  pure $ psTypeDecl "Array" [decl]
-- we currently assume that refs defined here are within the module
-- please add a test when this assumption no longer holds ;)
useTypeDecl (TypRef { name }) = pure $ psTypeDecl_ name

addDecls :: Array PSDecl -> Builder Unit
addDecls decls = modify_ \p -> p { declarations = p.declarations <> decls }

addComponent :: Boolean -> Component -> Builder Unit
addComponent isPrimary { name, props } = do
  addComponentImports
  importPropTyps
  addExport ( PSDeclNameType { name: pcn.propsName
                             , includeConstructors: false
                             }
            )
  addExport (PSDeclNameFun pcn.funName)
  rows <- traverse useRecordRow props
  addDecls
    [ PSDeclTypeRecord
      { name: pcn.propsName
      , rows
      }
    , PSDeclForeignRC pcn
    ]
  addJSExport { name: pcn.foreignComponentName, member: jsMember }

  where
    importPropTyps =
      traverse_ (usePropTypeDecl <<< _.propTyp) props

    pcn = mkCompNames name

    jsMember =
      if isPrimary
      then Nothing
      else Just name

addComponentImports :: Builder Unit
addComponentImports = do
  addImportType  "React.Basic" "ReactComponent" false
  addImportFun   "React.Basic" "element"
  addImportClass "Untagged.Coercible" "Coercible"
  addImportFun   "Untagged.Coercible" "coerce"

build :: String -> Builder Unit -> ModuleBundle
build name builder =
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

    finalState = execState builder initState

    -- note: Map.toUnfoldable already guarantees
    -- sorted keys
    imports = uncurry toPSImport <$> Map.toUnfoldable finalState.imports

    toPSImport mod namesSet =
      { mod
      , names: Set.toUnfoldable namesSet
      }


useRecordRow :: Prop -> Builder PSRecordRow
useRecordRow p =
  usePropTypeDecl p.propTyp <#> \typeDecl ->
  { name: p.name
  , typeDecl
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
