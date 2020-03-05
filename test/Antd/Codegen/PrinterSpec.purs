module Antd.Codegen.PrinterSpec
       ( printerSpec
       ) where

import Prelude

import Antd.Codegen.Printer (printModuleSection)
import Antd.Codegen.Types (PSDeclName(..), PSImport, PSModule)
import Data.String (Pattern(..), contains)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

printerSpec :: Spec Unit
printerSpec =
  describe "Printer" do
    describe "module section" do
      it "should have module name" do
        printModuleSection (mDef "Foo" [] []) `shouldSatisfy`
          contains (Pattern "module Foo")

        printModuleSection (mDef "Foo.Bar" [] []) `shouldSatisfy`
          contains (Pattern "module Foo.Bar")

      it "omit empty exports" do
        printModuleSection (mDef "Foo" [] []) `shouldEqual`
          ( "module Foo"
            <> "\n  where"
          )

      it "declare exports " do
        printModuleSection
          ( mDef "Foo"
            [ PSDeclNameFun "myFun"
            , PSDeclNameType { name: "MyType"
                             , includeConstructors: false
                             }
            , PSDeclNameType { name: "MyData"
                             , includeConstructors: true
                             }
            , PSDeclNameClass "MyClass"
            ]
            []
          )
          `shouldEqual`
          ( "module Foo"
            <> "\n  ( myFun"
            <> "\n  , MyType"
            <> "\n  , MyData(..)"
            <> "\n  , class MyClass"
            <> "\n  ) where"
          )

mDef :: String -> Array PSDeclName -> Array PSImport -> PSModule
mDef name exports imports = { name, exports, imports }
