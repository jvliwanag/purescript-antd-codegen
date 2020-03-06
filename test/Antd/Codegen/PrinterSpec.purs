module Antd.Codegen.PrinterSpec
       ( printerSpec
       ) where

import Prelude

import Antd.Codegen.Printer (printDecl, printImportSection, printModule, printModuleSection)
import Antd.Codegen.Types (PSDecl(..), PSDeclName(..), PSRecordRow)
import Data.Maybe (Maybe(..))
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
          , PSDeclNameType { name: "|+|"
                           , includeConstructors: false
                           }
          , PSDeclNameClass "MyClass"
          ]
          `shouldEqual`
          ( "module Foo"
            <> "\n  ( myFun"
            <> "\n  , MyType"
            <> "\n  , MyData(..)"
            <> "\n  , type (|+|)"
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

    describe "declarations" do
      it "should print empty record" do
        printDecl (
          PSDeclTypeRecord
          { name: "Foo"
          , rows: []
          }
          ) `shouldEqual`
          ( "type Foo"
            <> "\n  = {"
            <> "\n    }"
          )

      it "should print record type rows with docs" do
        printDecl (
          PSDeclTypeRecord
          { name: "Foo"
          , rows:
            [ recordRow "prop1" false "Int" Nothing
            , recordRow "prop2" false "Int" (Just "line1\nline2\nline3")
            , recordRow "prop3" false "Int" Nothing
            , recordRow "prop4" false "Int" Nothing
            , recordRow "prop5" false "Int" (Just "line1")
            , recordRow "prop6" false "Int" (Just "line1")
            ]
          }
          ) `shouldEqual`
          (      "type Foo"
            <> "\n  = { prop1 :: Int"
            <> "\n      -- line1"
            <> "\n      -- line2"
            <> "\n      -- line3"
            <> "\n    , prop2 :: Int"
            <> "\n    , prop3 :: Int"
            <> "\n    , prop4 :: Int"
            <> "\n      -- line1"
            <> "\n    , prop5 :: Int"
            <> "\n      -- line1"
            <> "\n    , prop6 :: Int"
            <> "\n    }"
          )
      it "should print record type starting with a row with doc" do
        printDecl (
          PSDeclTypeRecord
          { name: "Foo"
          , rows:
            [ recordRow "prop1" false "Int" (Just "line")
            ]
          }
          ) `shouldEqual`
          (      "type Foo"
            <> "\n  = { -- line"
            <> "\n      prop1 :: Int"
            <> "\n    }"
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

recordRow :: String -> Boolean -> String -> Maybe String -> PSRecordRow
recordRow name allowUndefined typ documentation =
  { name, allowUndefined, typ, documentation }
