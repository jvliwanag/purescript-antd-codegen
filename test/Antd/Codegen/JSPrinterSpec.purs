module Antd.Codegen.JSPrinterSpec
       ( jsPrinterSpec
       ) where

import Prelude

import Antd.Codegen.JSPrinter (printJSExports)
import Antd.Codegen.Types (jsExport)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

jsPrinterSpec :: Spec Unit
jsPrinterSpec =
  describe "JSPrinter" do
    it "should print a js binding" do
      printJSExports
        [ jsExport "_foo" "antd" ["Foo"]
        , jsExport "_bar" "antd" ["Foo", "Bar"]
        ]
        `shouldEqual`
        (    "exports._foo = require('antd').Foo;"
          <> "\nexports._bar = require('antd').Foo.Bar;"
          <> "\n"
        )
