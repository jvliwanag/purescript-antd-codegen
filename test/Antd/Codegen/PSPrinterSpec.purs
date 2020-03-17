module Antd.Codegen.PSPrinterSpec
       ( psPrinterSpec
       ) where

import Prelude

import Antd.Codegen.PSPrinter (printDecl, printImportSection, printModule, printModuleSection, printTypeDecl)
import Antd.Codegen.Types (PSDecl(..), PSDeclName(..), PSRecordRow, PSTypeDecl, psTypeArgSymbol, psTypeDecl, psTypeDecl', psTypeDeclOp, psTypeDeclRecord, psTypeDecl_)
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

    describe "type" do
      it "should print a type with without type args" do
        printTypeDecl (psTypeDecl_ "String") `shouldEqual` "String"

      it "should print a type with with type args" do
        printTypeDecl (psTypeDecl "EffectFn1" [ psTypeDecl_ "String", psTypeDecl_ "Unit" ])
          `shouldEqual` "EffectFn1 String Unit"

      it "should quote type arg with its own type arg" do
        printTypeDecl (psTypeDecl "Array" [ psTypeDecl "UndefinedOr" [ psTypeDecl_ "String" ] ] )
          `shouldEqual` "Array (UndefinedOr String)"

      it "should print types separated by a type operator" do
        printTypeDecl (psTypeDeclOp "|+|" [ psTypeDecl_ "Int", psTypeDecl_ "String", psTypeDecl_ "Boolean" ] )
          `shouldEqual` "Int |+| String |+| Boolean"

      it "should quote type args using type op" do
        printTypeDecl (psTypeDecl "Array" [(psTypeDeclOp "|+|" [ psTypeDecl_ "String", psTypeDecl_ "Int" ] )])
          `shouldEqual` "Array (String |+| Int)"

      it "should print symbol type arg" do
        printTypeDecl (psTypeDecl' "StringLiteral" [ psTypeArgSymbol "foo"
                                                   ])
          `shouldEqual` "StringLiteral \"foo\""

      it "should print record type decl" do
        printTypeDecl (psTypeDeclRecord
                       [ { name: "foo", typeDecl: psTypeDecl_ "String"}
                       , { name: "bar"
                         , typeDecl: psTypeDecl "Array"
                           [ psTypeDecl_ "String"
                           ]
                         }
                       , { name: "baz"
                         , typeDecl: psTypeDecl "Array"
                           [ psTypeDecl "UndefinedOr"
                             [ psTypeDecl_ "Int"
                             ]
                           ]
                         }
                       ])
          `shouldEqual` "{foo :: String, bar :: Array String, baz :: Array (UndefinedOr Int)}"

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
            [ recordRow "prop1" intTypeDecl Nothing
            , recordRow "prop2" intTypeDecl (Just "line1\nline2\nline3")
            , recordRow "prop3" intTypeDecl Nothing
            , recordRow "prop4" intTypeDecl Nothing
            , recordRow "prop5" intTypeDecl (Just "line1")
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
            <> "\n    }"
          )
      it "should print record type starting with a row with doc" do
        printDecl (
          PSDeclTypeRecord
          { name: "Foo"
          , rows:
            [ recordRow "prop1" intTypeDecl (Just "line")
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
                [ recordRow "text" (psTypeDecl "UndefinedOr"
                                    [ psTypeDeclOp "|+|"
                                      [ psTypeDecl_ "String"
                                      , psTypeDecl_ "JSX"
                                      ]
                                    ]) Nothing
                , recordRow "onClick" (psTypeDecl "UndefinedOr"
                                       [ psTypeDecl "Effect"
                                         [ psTypeDecl_ "Unit"
                                         ]
                                       ]) (Just "on click")
                , recordRow "children" (psTypeDecl "UndefinedOr"
                                       [ psTypeDecl "Array"
                                         [ psTypeDecl_ "JSX"
                                         ]
                                       ]) Nothing
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

recordRow :: String -> PSTypeDecl -> Maybe String -> PSRecordRow
recordRow name typeDecl documentation =
  { name, typeDecl, documentation }

intTypeDecl :: PSTypeDecl
intTypeDecl = psTypeDecl_ "Int"
