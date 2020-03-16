module Antd.Codegen.PSPrinterSpec
       ( psPrinterSpec
       ) where

import Prelude

import Antd.Codegen.PSPrinter (printDecl, printImportSection, printModule, printModuleSection)
import Antd.Codegen.Types (PSDecl(..), PSDeclName(..), PSRecordRow)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

psPrinterSpec :: Spec Unit
psPrinterSpec =
  describe "PSPrinter" do
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
            <> "\nimport Foo (fooFun)"
            <> "\nimport Bar (BarType, BarData(..))"
            <> "\nimport Baz (class BazClass)"
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
            [ recordRow "prop1" "Int" Nothing
            , recordRow "prop2" "Int" (Just "line1\nline2\nline3")
            , recordRow "prop3" "Int" Nothing
            , recordRow "prop4" "Int" Nothing
            , recordRow "prop5" "Int" (Just "line1")
            , recordRow "prop6" "UndefinedOr Int" (Just "line1")
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
            [ recordRow "prop1" "Int" (Just "line")
            ]
          }
          ) `shouldEqual`
          (      "type Foo"
            <> "\n  = { -- line"
            <> "\n      prop1 :: Int"
            <> "\n    }"
          )

      it "should print foreign react components" do
        printDecl
          ( PSDeclForeignRC
            { funName: "foo"
            , foreignComponentName: "_foo"
            , propsName: "FooProps"
            }
          ) `shouldEqual`
          ( "foreign import _foo :: ReactComponent FooProps"
            <> "\n"
            <> "\nfoo :: forall r. Coercible r FooProps => r -> JSX"
            <> "\nfoo props = element _foo (coerce props)"
          )

    describe "printer" do
      it "should print module" do
        printModule
          { name: "Antd.Foo"
          , exports:
            [ PSDeclNameType { includeConstructors: false
                             , name: "FooProps"
                             }
            , PSDeclNameFun "foo"
            ]
          , importPrelude: true
          , imports:
            [ { mod: "Effect"
              , names: [ PSDeclNameType { includeConstructors: false
                                        , name: "Effect"
                                        }
                       ]
              }
            , { mod: "Literals"
              , names: [ PSDeclNameType { includeConstructors: false
                                        , name: "StringLit"
                                        }
                       ]
              }
            , { mod: "React.Basic"
              , names: [ PSDeclNameType { includeConstructors: false
                                        , name: "JSX"
                                        }
                       , PSDeclNameType { includeConstructors: false
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
            , { mod: "Untagged.Union"
              , names: [ PSDeclNameType { includeConstructors: false
                                        , name: "|+|"
                                        }
                       , PSDeclNameType { includeConstructors: false
                                        , name: "UndefinedOr"
                                        }
                       ]
              }
            ]
          , declarations:
            [ PSDeclTypeRecord
              { name: "FooProps"
              , rows:
                [ recordRow "text" "UndefinedOr (String |+| JSX)" Nothing
                , recordRow "onClick" "UndefinedOr (Effect Unit)" (Just "on click")
                , recordRow "children" "UndefinedOr (Array JSX)" Nothing
                ]
              }
            , PSDeclForeignRC { funName: "foo"
                              , foreignComponentName: "_foo"
                              , propsName: "FooProps"
                              }
            ]
          } `shouldEqual`
          (       "module Antd.Foo"
            <> "\n  ( FooProps"
            <> "\n  , foo"
            <> "\n  ) where"
            <> "\n"
            <> "\nimport Prelude"
            <> "\n"
            <> "\nimport Effect (Effect)"
            <> "\nimport Literals (StringLit)"
            <> "\nimport React.Basic (JSX, ReactComponent, element)"
            <> "\nimport Untagged.Coercible (class Coercible, coerce)"
            <> "\nimport Untagged.Union (type (|+|), UndefinedOr)"
            <> "\n"
            <> "\ntype FooProps"
            <> "\n  = { text :: UndefinedOr (String |+| JSX)"
            <> "\n      -- on click"
            <> "\n    , onClick :: UndefinedOr (Effect Unit)"
            <> "\n    , children :: UndefinedOr (Array JSX)"
            <> "\n    }"
            <> "\n"
            <> "\nforeign import _foo :: ReactComponent FooProps"
            <> "\n"
            <> "\nfoo :: forall r. Coercible r FooProps => r -> JSX"
            <> "\nfoo props = element _foo (coerce props)"
            <> "\n"
          )

recordRow :: String -> String -> Maybe String -> PSRecordRow
recordRow name typ documentation =
  { name, typ, documentation }
