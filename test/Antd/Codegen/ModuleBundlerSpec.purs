module Antd.Codegen.ModuleBundlerSpec
       ( moduleBundlerSpec
       ) where

import Prelude

import Antd.Codegen.ModuleBundler (createPSModule)
import Antd.Codegen.Types (PSDecl(..), PSDeclName(..), PSImport, PSModule, Prop, Typ(..), optionalPropTyp, prop, requiredPropTyp)
import Data.Array as Array
import Data.Maybe (isJust)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

moduleBundlerSpec :: Spec Unit
moduleBundlerSpec =
  describe "ModuleBundler" do
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

    it "should import prelude when needed" do
      ( createFooPSModule
       [ prop "bar" $ requiredPropTyp TypInt
       ]
      ).importPrelude `shouldEqual` true

    it "should import UndefinedOr when needed" do
      ( createFooPSModule
       [ prop "bar" $ optionalPropTyp TypInt
       ]
      ).imports `shouldContainImport` (importType "Untagged.Union" "UndefinedOr")

emptyModule :: PSModule
emptyModule = createFooPSModule []

createFooPSModule :: Array Prop -> PSModule
createFooPSModule primaryProps =
  createPSModule
  { primaryComponent:
    { name: "Foo"
    , props: primaryProps
    }
  , subComponents: []
  }

importType :: String -> String -> { mod :: String, name :: PSDeclName }
importType mod name = { mod: "Untagged.Union", name: (PSDeclNameType { name, includeConstructors: false }) }

shouldContainImport :: Array PSImport -> { mod :: String, name :: PSDeclName } -> Aff Unit
shouldContainImport imports { mod, name } =
  containsImport `shouldEqual` true

  where
    containsImport :: Boolean
    containsImport =
      isJust $ Array.find (\i -> i.mod == mod && Array.elem name i.names) imports
