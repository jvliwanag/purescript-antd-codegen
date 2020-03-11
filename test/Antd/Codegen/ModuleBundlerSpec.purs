module Antd.Codegen.ModuleBundlerSpec
       ( moduleBundlerSpec
       ) where

import Prelude

import Antd.Codegen.ModuleBundler (createPSModule)
import Antd.Codegen.Types (PSDecl(..), PSDeclName(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

moduleBundlerSpec :: Spec Unit
moduleBundlerSpec =
  describe "ModuleBundler" do
    it "should create a ps module with no props" do
      createPSModule
        { name: "Foo"
        , primaryProps: []
        , subComponents: []
        } `shouldEqual`
        { name: "Antd.Foo"
        , exports:
          [ PSDeclNameType { name: "FooProps", includeConstructors: false }
          , PSDeclNameFun "foo"
          ]
        , importPrelude: false
        , imports:
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
        , declarations:
          [ PSDeclTypeRecord
              { name: "FooProps"
              , rows: []
              }
          , PSDeclForeignRC { funName: "foo"
                            , foreignComponentName: "_foo"
                            , propsName: "FooProps"
                            }
          ]
        }