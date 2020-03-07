module Antd.Codegen.PrinterSpec
       ( printerSpec
       ) where

import Prelude

import Antd.Codegen.Printer (printDecl, printImportSection, printModule, printModuleSection, printTyp)
import Antd.Codegen.Types (PSDecl(..), PSDeclName(..), PSRecordRow, Typ(..), optionalPropTyp, requiredPropTyp)
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
            [ recordRow "prop1" true TypInt Nothing
            , recordRow "prop2" true TypInt (Just "line1\nline2\nline3")
            , recordRow "prop3" true TypInt Nothing
            , recordRow "prop4" true TypInt Nothing
            , recordRow "prop5" true TypInt (Just "line1")
            , recordRow "prop6" false TypInt (Just "line1")
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
            <> "\n    , prop6 :: UndefinedOr Int"
            <> "\n    }"
          )
      it "should print record type starting with a row with doc" do
        printDecl (
          PSDeclTypeRecord
          { name: "Foo"
          , rows:
            [ recordRow "prop1" true TypInt (Just "line")
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

recordRow :: String -> Boolean -> Typ -> Maybe String -> PSRecordRow
recordRow name required typ documentation =
  { name, propTyp: { required,  typ } , documentation }

itShouldPrintTyp :: Typ -> String -> Spec Unit
itShouldPrintTyp typ expected = do
  it ("should print typ " <> show typ) do
    printTyp typ `shouldEqual` expected
