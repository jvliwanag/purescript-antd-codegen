module Antd.Codegen.PrinterSpec
       ( printerSpec
       ) where

import Prelude

import Antd.Codegen.Printer (printImportSection, printModule, printModuleSection)
import Antd.Codegen.Types (PSDeclName(..))
import Data.String (Pattern(..), contains)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

printerSpec :: Spec Unit
printerSpec =
  describe "Printer" do
    describe "module section" do
      it "should have module name" do
        printModuleSection "Foo" [] `shouldSatisfy`
          contains (Pattern "module Foo")

        printModuleSection "Foo.Bar" [] `shouldSatisfy`
          contains (Pattern "module Foo.Bar")

      it "omit empty exports" do
        printModuleSection "Foo" [] `shouldEqual`
          ( "module Foo"
            <> "\n  where"
          )

      it "declare exports" do
        printModuleSection "Foo"
          [ PSDeclNameFun "myFun"
          , PSDeclNameType { name: "MyType"
                           , includeConstructors: false
                           }
          , PSDeclNameType { name: "MyData"
                           , includeConstructors: true
                           }
          , PSDeclNameClass "MyClass"
          ]
          `shouldEqual`
          ( "module Foo"
            <> "\n  ( myFun"
            <> "\n  , MyType"
            <> "\n  , MyData(..)"
            <> "\n  , class MyClass"
            <> "\n  ) where"
          )

    describe "imports" do
      it "should allow import Prelude" do
        printImportSection true [] `shouldEqual`
          "import Prelude\n"

      it "should import modules" do
        printImportSection true
          [ { mod: "Foo", names: [ PSDeclNameFun "fooFun" ] }
          , { mod: "Bar"
            , names: [ PSDeclNameType { name: "BarType"
                                      , includeConstructors: false
                                      }
                     , PSDeclNameType { name: "BarData"
                                      , includeConstructors: true
                                      }
                     ]
            }
          , { mod: "Baz"
            , names: [ PSDeclNameClass "BazClass"
                     ]
            }
          ] `shouldEqual`
          ( "import Prelude"
            <> "\n"
            <> "\nimport Foo(fooFun)"
            <> "\nimport Bar(BarType, BarData(..))"
            <> "\nimport Baz(class BazClass)"
          )

    describe "printer" do
      it "should print module" do
        printModule
          { name: "Foo.Bar"
          , exports:
            [ PSDeclNameFun "myFun"
            ]
          , importPrelude: true
          , imports:
            [ { mod: "Foo"
              , names: [ PSDeclNameFun "fooFun" ]
              }
            ]
          } `shouldEqual`
          ( "module Foo.Bar"
            <> "\n  ( myFun"
            <> "\n  ) where"
            <> "\n"
            <> "\nimport Prelude"
            <> "\n"
            <> "\nimport Foo(fooFun)"
            <> "\n"
          )
