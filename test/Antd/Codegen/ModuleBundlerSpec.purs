module Antd.Codegen.ModuleBundlerSpec
       ( moduleBundlerSpec
       ) where

import Prelude

import Antd.Codegen.ModuleBundler (createModuleBundle)
import Antd.Codegen.Types (ModuleBundle, PSDecl(..), PSDeclName(..), PSModule, PSTypeDecl, Prop, PropTyp, Typ(..), optionalPropTyp, prop, psTypeDecl, psTypeDeclOp, psTypeDecl_, requiredPropTyp)
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
      (moduleWithRequiredTyp TypUnknown)
        `shouldContainImport`
        (importType "Foreign" "Foreign")

    it "should import StringLit" do
      (moduleWithRequiredTyp (TypStringLit "foo"))
        `shouldContainImport`
        (importType "Literals" "StringLit")

    it "should import BooleanLit" do
      (moduleWithRequiredTyp (TypBooleanLit true))
        `shouldContainImport`
        (importType "Literals" "BooleanLit")

    it "should support JSX" do
      let mod = moduleWithRequiredTyp TypNode
      mod `shouldContainImport` (importType "React.Basic" "JSX")
      mod `shouldHaveProp` (psTypeDecl_ "JSX")

    it "should support |+| and inner types for oneof" do
      let mod = moduleWithRequiredTyp (TypOneOf [ TypNode, TypInt ])
      mod `shouldContainImport` (importType "Untagged.Union" "|+|")

      -- for node
      mod `shouldContainImport` (importType "React.Basic" "JSX")

      -- for Int
      mod.importPrelude `shouldEqual` true

      mod `shouldHaveProp` (psTypeDeclOp "|+|" [psTypeDecl_ "JSX", psTypeDecl_ "Int"])

    it "should import FnX and inner type for non effectful function" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: false
                                             , input: [ requiredPropTyp TypInt
                                                      , requiredPropTyp TypString
                                                      , requiredPropTyp TypUnknown
                                                      ]
                                             , output: requiredPropTyp TypNode
                                             })
      mod `shouldContainImport` (importType "Data.Function.Uncurried" "Fn3")

      -- for inputs
      mod.importPrelude `shouldEqual` true
      mod `shouldContainImport` (importType "Foreign" "Foreign")

      -- for output
      mod `shouldContainImport` (importType "React.Basic" "JSX")

      mod `shouldHaveProp` (psTypeDecl "Fn3" [ psTypeDecl_ "Int"
                                             , psTypeDecl_ "String"
                                             , psTypeDecl_ "Foreign"
                                             , psTypeDecl_ "JSX"
                                             ])

    it "should support EffectFnX for effectful function" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: true
                                             , input: [ requiredPropTyp TypInt
                                                      , requiredPropTyp TypString
                                                      , requiredPropTyp TypUnknown
                                                      ]
                                             , output: requiredPropTyp TypNode
                                             })
      mod `shouldContainImport` (importType "Effect.Uncurried" "EffectFn3")

      mod `shouldHaveProp` (psTypeDecl "EffectFn3" [ psTypeDecl_ "Int"
                                                   , psTypeDecl_ "String"
                                                   , psTypeDecl_ "Foreign"
                                                   , psTypeDecl_ "JSX"
                                                   ])

    it "should use normal (->) for non effectful function with lt 2 arguments" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: false
                                             , input: [ requiredPropTyp TypInt ]
                                             , output: requiredPropTyp TypNode
                                             })
      (mod.imports # Array.find (\i -> i.mod == "Data.Function.Uncurried")) `shouldSatisfy` isNothing

      mod `shouldHaveProp` (psTypeDeclOp "->" [ psTypeDecl_ "Int", psTypeDecl_ "JSX" ])

    it "should add a unit input for nont effectful function with no arguments" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: false
                                             , input: []
                                             , output: requiredPropTyp TypNode
                                             })
      mod.importPrelude `shouldEqual` true
      mod `shouldHaveProp` (psTypeDeclOp "->" [ psTypeDecl_ "Unit", psTypeDecl_ "JSX" ])


    it "should use Effect for effectful function no args" do
      let mod = moduleWithRequiredTyp (TypFn { effectful: true
                                             , input: []
                                             , output: requiredPropTyp TypNode
                                             })
      mod `shouldContainImport` (importType "Effect" "Effect")
      mod `shouldHaveProp` (psTypeDecl "Effect" [ psTypeDecl_ "JSX" ])

    it "should import types required for record fields" do
      let mod = moduleWithRequiredTyp
                (TypRecord [{ key: "k"
                            , propTyp: requiredPropTyp TypInt
                            }])
      mod.importPrelude `shouldEqual` true

    it "should import types required for array values and have an array prop" do
      let mod = moduleWithRequiredTyp (TypArray TypInt)
      mod.importPrelude `shouldEqual` true

      mod `shouldHaveProp` (psTypeDecl "Array" [ psTypeDecl_ "Int" ])

    it "should use UndefinedOr and inner type for optional props" do
      let mod = moduleWithPropTyp (optionalPropTyp TypInt)
      mod `shouldContainImport` (importType "Untagged.Union" "UndefinedOr")
      mod.importPrelude `shouldEqual` true

      mod `shouldHaveProp` (psTypeDecl "UndefinedOr" [ psTypeDecl_ "Int" ])

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

shouldContainImport :: PSModule -> { mod :: String, name :: PSDeclName } -> Aff Unit
shouldContainImport { imports } { mod, name } = do
  containsImport `shouldEqual` true

  where
    containsImport :: Boolean
    containsImport =
      isJust $ Array.find (\i -> i.mod == mod && Array.elem name i.names) imports

shouldHaveProp :: PSModule -> PSTypeDecl -> Aff Unit
shouldHaveProp { declarations } typeDecl =
  rowTypeDecl `shouldContain` typeDecl
  where
    rowTypeDecl = declarations # Array.findMap case _ of
      PSDeclTypeRecord { rows: [f] } -> Just f.typeDecl
      _ -> Nothing

itShouldImportPreludeForTyp :: Typ -> Spec Unit
itShouldImportPreludeForTyp typ =
  it ("should import prelude for " <> show typ) do
    ( createFooPSModule
      [ prop "bar" $ requiredPropTyp typ
      ]
    ).importPrelude `shouldEqual` true


{-

    describe "types" do
      itShouldPrintTyp TypInt "Int"
      itShouldPrintTyp TypString "String"
      itShouldPrintTyp TypInt "Int"
      itShouldPrintTyp TypNumber "Number"
      itShouldPrintTyp TypBoolean "Boolean"
      itShouldPrintTyp (TypRef { name: "Foo" }) "Foo"
      itShouldPrintTyp TypUnknown "Foreign"
      itShouldPrintTyp (TypStringLit "Foo") "StringLit \"Foo\""
      itShouldPrintTyp (TypBooleanLit true) "BooleanLit \"true\""
      itShouldPrintTyp (TypBooleanLit false) "BooleanLit \"false\""
      itShouldPrintTyp TypNode "JSX"
      itShouldPrintTyp TypUnit "Unit"

      -- oneof
      itShouldPrintTyp (TypOneOf [TypInt, TypString, TypNode]) $
        "Int |+| String |+| JSX"

      -- array
      itShouldPrintTyp (TypArray TypInt) $
        "Array Int"
      itShouldPrintTyp (TypArray (TypOneOf [TypInt, TypString])) $
        "Array (Int |+| String)"

      -- todo allow input as undefinedor
      -- fn
      itShouldPrintTyp (TypFn { effectful: false
                              , input: []
                              , output: requiredPropTyp TypInt
                              }) $
        "Unit -> Int"

      itShouldPrintTyp (TypFn { effectful: false
                              , input: [ requiredPropTyp TypString
                                       ]
                              , output: requiredPropTyp TypInt
                              }) $
        "String -> Int"

      itShouldPrintTyp (TypFn { effectful: false
                              , input: [ requiredPropTyp TypString
                                       , requiredPropTyp TypBoolean
                                       ]
                              , output: requiredPropTyp (TypArray TypInt)
                              }) $
        "Fn2 String Boolean (Array Int)"

      itShouldPrintTyp (TypFn { effectful: true
                              , input: []
                              , output: requiredPropTyp TypInt
                              }) $
        "Effect Int"

      itShouldPrintTyp (TypFn { effectful: true
                              , input: [ requiredPropTyp TypString
                                       ]
                              , output: requiredPropTyp TypInt
                              }) $
        "EffectFn1 String Int"

      itShouldPrintTyp (TypFn { effectful: true
                              , input: [ requiredPropTyp TypString
                                       , requiredPropTyp TypBoolean
                                       ]
                              , output: requiredPropTyp (TypArray TypInt)
                              }) $
        "EffectFn2 String Boolean (Array Int)"

      -- optional in fn
      itShouldPrintTyp (TypFn { effectful: false
                              , input: [ optionalPropTyp TypString
                                       , requiredPropTyp TypBoolean
                                       ]
                              , output: optionalPropTyp (TypArray TypInt)
                              }) $
        "Fn2 (UndefinedOr String) Boolean (UndefinedOr (Array Int))"

      -- record
      itShouldPrintTyp (TypRecord []) "{}"

      itShouldPrintTyp
        ( TypRecord
          [ { key: "foo", propTyp: optionalPropTyp TypString }
          , { key: "bar", propTyp: requiredPropTyp TypBoolean }
          , { key: "baz"
            , propTyp: optionalPropTyp (TypOneOf [ TypString
                                                 , TypBoolean
                                                 ]
                                       )
            }
          ]
        ) $
        "{ foo :: UndefinedOr String, bar :: Boolean, baz :: UndefinedOr (String |+| Boolean) }"


itShouldPrintTyp :: Typ -> String -> Spec Unit
itShouldPrintTyp typ expected = do
  it ("should print typ " <> show typ) do
    printTyp typ `shouldEqual` expected

-}
