module Antd.Codegen.ModuleBundlerSpec
       ( moduleBundlerSpec
       ) where

import Prelude

import Antd.Codegen.ModuleBundler (createModuleBundle)
import Antd.Codegen.Types (PSDecl(..), PSDeclName(..), PSImport, PSModule, Prop, PropTyp, Typ(..), ModuleBundle, optionalPropTyp, prop, requiredPropTyp)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)

moduleBundlerSpec :: Spec Unit
moduleBundlerSpec =
  describe "ModuleBundler" do
    it "should have the correct bundle name" do
      emptyModuleBundle.name `shouldEqual` "Foo"

    it "should create a ps module with a proper name" do
      emptyModule.name `shouldEqual` "Antd.Foo"

    it "should create an empty ps module with minimal import" do
      emptyModule.importPrelude `shouldEqual` false
      emptyModule.imports `shouldEqual`
        [ { mod: "React.Basic"
          , names: [ PSDeclNameType { includeConstructors: false
                                    , name: "ReactComponent"
                                    }
                   , PSDeclNameFun "element"
                   ]
          }
        , { mod: "Untagged.Coercible"
          , names: [ PSDeclNameClass "Coercible"
                   , PSDeclNameFun "coerce"
                   ]
          }
        ]

    it "should create an empty ps module with exports" do
      emptyModule.exports `shouldEqual`
        [ PSDeclNameType { name: "FooProps", includeConstructors: false }
        , PSDeclNameFun "foo"
        ]

    it "should create an empty ps module with declarations" do
      emptyModule.declarations `shouldEqual`
        [ PSDeclTypeRecord
              { name: "FooProps"
              , rows: []
              }
        , PSDeclForeignRC { funName: "foo"
                          , foreignComponentName: "_foo"
                          , propsName: "FooProps"
                          }
        ]

    traverse_ itShouldImportPreludeForTyp
      [ TypString
      , TypInt
      , TypNumber
      , TypBoolean
      , TypUnit
      ]

    it "should import foreign" do
      (moduleWithRequiredTyp TypUnknown).imports
        `shouldContainImport`
        (importType "Foreign" "Foreign")

    it "should import StringLit" do
      (moduleWithRequiredTyp (TypStringLit "foo")).imports
        `shouldContainImport`
        (importType "Literals" "StringLit")

    it "should import BooleanLit" do
      (moduleWithRequiredTyp (TypBooleanLit true)).imports
        `shouldContainImport`
        (importType "Literals" "BooleanLit")

    it "should import JSX" do
      (moduleWithRequiredTyp TypNode).imports
        `shouldContainImport`
        (importType "React.Basic" "JSX")

    it "should import |+| and inner type for oneof" do
      let mod = moduleWithRequiredTyp (TypOneOf [ TypNode, TypInt ])
      mod.imports `shouldContainImport` (importType "Untagged.Union" "|+|")

      -- for node
      mod.imports `shouldContainImport` (importType "React.Basic" "JSX")

      -- for Int
      mod.importPrelude `shouldEqual` true

    it "should import FnX and inner type for non effectful function" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: false
                                             , input: [ requiredPropTyp TypInt
                                                      , requiredPropTyp TypString
                                                      , requiredPropTyp TypUnknown
                                                      ]
                                             , output: requiredPropTyp TypNode
                                             })
      mod.imports `shouldContainImport` (importType "Data.Function.Uncurried" "Fn3")

      -- for inputs
      mod.importPrelude `shouldEqual` true
      mod.imports `shouldContainImport` (importType "Foreign" "Foreign")

      -- for output
      mod.imports `shouldContainImport` (importType "React.Basic" "JSX")

    it "should import EffectFnX and inner type for effectful function" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: true
                                             , input: [ requiredPropTyp TypInt
                                                      , requiredPropTyp TypString
                                                      , requiredPropTyp TypUnknown
                                                      ]
                                             , output: requiredPropTyp TypNode
                                             })
      mod.imports `shouldContainImport` (importType "Effect.Uncurried" "EffectFn3")

    it "should not import uncurried for non effectful function with lt 2 arguments" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: false
                                             , input: [ requiredPropTyp TypInt ]
                                             , output: requiredPropTyp TypNode
                                             })
      (mod.imports # Array.find (\i -> i.mod == "Data.Function.Uncurried")) `shouldSatisfy` isNothing

    it "should not import preldue for non effectful function with no arguments" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: false
                                             , input: []
                                             , output: requiredPropTyp TypNode
                                             })
      mod.importPrelude `shouldEqual` true


    it "should import Effect for effectful function no args" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: true
                                             , input: []
                                             , output: requiredPropTyp TypNode
                                             })
      mod.imports `shouldContainImport` (importType "Effect" "Effect")


    it "should import types required for record fields" do
      let mod = moduleWithRequiredTyp
                (TypRecord [{ key: "k"
                            , propTyp: requiredPropTyp TypInt
                            }])
      mod.importPrelude `shouldEqual` true

    it "should import types required for array values" do
      (moduleWithRequiredTyp (TypArray TypInt)).importPrelude
        `shouldEqual` true

    it "should import UndefinedOr and inner type for optional props" do
      let mod = moduleWithPropTyp (optionalPropTyp TypInt)
      mod.imports `shouldContainImport` (importType "Untagged.Union" "UndefinedOr")
      mod.importPrelude `shouldEqual` true

    it "should include subComponents" do
      ( createModuleBundle
        { primaryComponent: { name: "Foo", props: [] }
        , subComponents: [ { name: "Bar", props: [] } ]
        }
      ).psModule.exports `shouldContain` (PSDeclNameFun "bar")

    it "should have js bindings for primary and sub components" do
      ( createModuleBundle
          { primaryComponent:
            { name: "Foo"
            , props: []
            }
          , subComponents:
            [ { name: "Bar"
              , props: []
              }
            ]
          }
      ).jsBinding `shouldEqual`
        { antSubmodule: "Foo"
        , exports:
          [ { name: "_foo", member: Nothing }
          , { name: "_bar", member: Just "Bar" }
          ]
        }

emptyModule :: PSModule
emptyModule = emptyModuleBundle.psModule

emptyModuleBundle :: ModuleBundle
emptyModuleBundle = createFooModuleBundle []

moduleWithRequiredTyp :: Typ -> PSModule
moduleWithRequiredTyp =
  moduleWithPropTyp <<< requiredPropTyp

moduleWithPropTyp :: PropTyp -> PSModule
moduleWithPropTyp propTyp =
  createFooPSModule [ prop "bar" propTyp
                    ]

createFooPSModule :: Array Prop -> PSModule
createFooPSModule =
  _.psModule <<< createFooModuleBundle

createFooModuleBundle :: Array Prop -> ModuleBundle
createFooModuleBundle primaryProps =
  createModuleBundle
  { primaryComponent:
    { name: "Foo"
    , props: primaryProps
    }
  , subComponents: []
  }

importType :: String -> String -> { mod :: String, name :: PSDeclName }
importType mod name = { mod, name: (PSDeclNameType { name, includeConstructors: false }) }

shouldContainImport :: Array PSImport -> { mod :: String, name :: PSDeclName } -> Aff Unit
shouldContainImport imports { mod, name } = do
  containsImport `shouldEqual` true

  where
    containsImport :: Boolean
    containsImport =
      isJust $ Array.find (\i -> i.mod == mod && Array.elem name i.names) imports

itShouldImportPreludeForTyp :: Typ -> Spec Unit
itShouldImportPreludeForTyp typ =
  it ("should import prelude for " <> show typ) do
    ( createFooPSModule
      [ prop "bar" $ requiredPropTyp typ
      ]
    ).importPrelude `shouldEqual` true
