module Test.Main where

import Prelude

import Antd.Codegen.JSPrinterSpec (jsPrinterSpec)
import Antd.Codegen.ModuleBundlerSpec (moduleBundlerSpec)
import Antd.Codegen.PSPrinterSpec (psPrinterSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  psPrinterSpec
  jsPrinterSpec
  moduleBundlerSpec
