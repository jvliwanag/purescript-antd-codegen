module Antd.Codegen.ModuleBundler
       ( createModuleBundle
       , ModuleBuilder
       , buildModule
       , buildModule_
       ) where

import Prelude

import Antd.Codegen.PropsBuilder (PropsBuilder, buildProps)
import Antd.Codegen.Types (AntModule, JSExport, ModuleBundle, PSDecl(..), PSDeclName(..), PSRecordRow, PSTypeDecl, Prop, Typ(..), psTypeArgSymbol, psTypeDecl, psTypeDecl', psTypeDeclOp, psTypeDeclRecord, psTypeDecl_)
import Control.Monad.State (class MonadState, State, execState, modify_)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (uncurry)

createModuleBundle :: AntModule -> ModuleBundle
createModuleBundle am =
  buildModule am.primaryComponent.name
  $ addAntComponent true am.primaryComponent
  *> traverse_ (addAntComponent false) am.subComponents
  where
    addAntComponent isPrimary { name, props } =
      addComponent name "antd" jsMember props
      where
        jsMember = if isPrimary
                   then [name]
                   else [am.primaryComponent.name, name]

newtype ModuleBuilder a = MB (State BuilderState a)
derive instance moduleBuilderNewtype :: Newtype (ModuleBuilder a) _

derive newtype instance moduleBuilderFunctor :: Functor ModuleBuilder
derive newtype instance moduleBuilderApply :: Apply ModuleBuilder
derive newtype instance moduleBuilderApplicative :: Applicative ModuleBuilder
derive newtype instance moduleBuilderBind :: Bind ModuleBuilder
derive newtype instance moduleBuilderMonad :: Monad ModuleBuilder
derive newtype instance moduleBuilderMonadState :: MonadState
                        ({ importPrelude :: Boolean
                         , exports :: Array PSDeclName
                         , imports :: Map String (Set PSDeclName)
                         , declarations :: Array PSDecl
                         , jsExports :: Array { name :: String
                                              , jsRequire :: String
                                              , jsPath :: Array String
                                              }
                         }) ModuleBuilder

type BuilderState =
  { importPrelude :: Boolean
  , exports :: Array PSDeclName
  , imports :: Map String (Set PSDeclName)
  , declarations :: Array PSDecl
  , jsExports :: Array JSExport
  }

buildModule :: String -> ModuleBuilder Unit -> ModuleBundle
buildModule name builder =
  { name
  , psModule:
    { name: "Antd." <> name
    , exports: finalState.exports
    , importPrelude: finalState.importPrelude
    , imports
    , declarations: finalState.declarations
    }
  , jsExports: finalState.jsExports
  }


  where
    initState =
      { importPrelude: false
      , exports: mempty
      , imports: mempty
      , declarations: mempty
      , jsExports: mempty
      }

    finalState = execState (unwrap builder) initState

    -- note: Map.toUnfoldable already guarantees
    -- sorted keys
    imports = uncurry toPSImport <$> Map.toUnfoldable finalState.imports

    toPSImport mod namesSet =
      { mod
      , names: Set.toUnfoldable namesSet
      }

buildModule_ :: String -> ModuleBundle
buildModule_ name = buildModule name (pure unit)

-- Antd Utils

addAntdReactComponent :: String -> PropsBuilder Unit -> ModuleBuilder Unit
addAntdReactComponent name propsB =
  addComponent name "antd" [name] (buildProps propsB)

-- React

addComponent :: String -> String -> Array String -> Array Prop -> ModuleBuilder Unit
addComponent name jsRequire jsPath props = do
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
  addJSExport { name: pcn.foreignComponentName
              , jsRequire
              , jsPath
              }

  where
    importPropTyps =
      traverse_ (useTypeDecl <<< _.typ) props

    pcn = mkCompNames name

    addComponentImports = do
      addImportType  "React.Basic" "ReactComponent" false
      addImportFun   "React.Basic" "element"
      addImportClass "Untagged.Coercible" "Coercible"
      addImportFun   "Untagged.Coercible" "coerce"

-- High level

addImportType :: String -> String -> Boolean -> ModuleBuilder Unit
addImportType mod name includeConstructors =
  addImport mod $ PSDeclNameType { name, includeConstructors }

addImportFun :: String -> String -> ModuleBuilder Unit
addImportFun mod name =
  addImport mod $ PSDeclNameFun name

addImportClass :: String -> String -> ModuleBuilder Unit
addImportClass mod name =
  addImport mod $ PSDeclNameClass name

useRecordRow :: Prop -> ModuleBuilder PSRecordRow
useRecordRow p =
  useTypeDecl p.typ <#> \typeDecl ->
  { name: p.name
  , typeDecl
  , doc: p.doc
  }

-- Raw Modifiers

addImportPrelude :: ModuleBuilder Unit
addImportPrelude = modify_ _ { importPrelude = true }

addImport :: String -> PSDeclName -> ModuleBuilder Unit
addImport mod name =
  modify_ \p -> p { imports = newImports p
                        }
  where
    newImports p = Map.alter (Just <<< updateModEntry) mod p.imports

    updateModEntry Nothing = Set.singleton name
    updateModEntry (Just p) = Set.insert name p


addExport :: PSDeclName -> ModuleBuilder Unit
addExport name =
  modify_ \p -> p { exports = Array.snoc p.exports name
                  }

addJSExport :: JSExport -> ModuleBuilder Unit
addJSExport b =
  modify_ \p ->p { jsExports = Array.snoc p.jsExports b
                 }

addExportType :: String -> Boolean -> ModuleBuilder Unit
addExportType name includeConstructors =
  addExport $ PSDeclNameType { name, includeConstructors }

addExportFun :: String -> ModuleBuilder Unit
addExportFun name =
  addExport $ PSDeclNameFun name

useTypeDecl :: Typ -> ModuleBuilder PSTypeDecl
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
useTypeDecl (TypUndefinedOr t) = do
  addImportType "Untagged.Union" "UndefinedOr" false
  d <- useTypeDecl t
  pure $ psTypeDecl "UndefinedOr" [d]
useTypeDecl (TypOneOf options) = do
  addImportType "Untagged.Union" "|+|" false
  psTypeDeclOp "|+|" <$> traverse useTypeDecl options
useTypeDecl (TypFn { effectful, input, output }) = do
  fn <- importUncurried
  outArg <- useTypeDecl output
  inArgs <- traverse useTypeDecl input
  pure $ fn $ Array.snoc inArgs outArg

  where
    importUncurried :: ModuleBuilder (Array PSTypeDecl -> PSTypeDecl)
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
    addRow { name, typ } =
      useTypeDecl typ <#> { name, typeDecl: _ }
useTypeDecl (TypArray a) = do
  decl <- useTypeDecl a
  pure $ psTypeDecl "Array" [decl]
-- we currently assume that refs defined here are within the module
-- please add a test when this assumption no longer holds ;)
useTypeDecl (TypRef { name }) = pure $ psTypeDecl_ name

addDecls :: Array PSDecl -> ModuleBuilder Unit
addDecls decls = modify_ \p -> p { declarations = p.declarations <> decls }

-- Misc Util

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
