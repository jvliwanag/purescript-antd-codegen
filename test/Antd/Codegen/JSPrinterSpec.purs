module Antd.Codegen.JSPrinterSpec
       ( jsPrinterSpec
       ) where

import Prelude

import Antd.Codegen.JSPrinter (printJSBinding)
import Antd.Codegen.Types (jsExport)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

jsPrinterSpec :: Spec Unit
jsPrinterSpec =
  describe "JSPrinter" do
    it "should print a js binding" do
      printJSBinding
        { antSubmodule: "Foo"
        , exports:
          [ jsExport "_foo" Nothing
          , jsExport "_bar" (Just "Bar")
          ]
        } `shouldEqual`
        (      "const Foo = require('antd').Foo;"
          <> "\n"
          <> "\nexports._foo = Foo;"
          <> "\nexports._bar = Foo.Bar;"
          <> "\n"
        )
