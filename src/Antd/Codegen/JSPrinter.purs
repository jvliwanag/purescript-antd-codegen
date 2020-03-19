module Antd.Codegen.JSPrinter
       ( printJSExports
       ) where

import Prelude

import Antd.Codegen.Types (JSExport)
import Data.Array as Array

printJSExports :: Array JSExport -> String
printJSExports exports =
  Array.foldMap ((_ <> "\n") <<< printExportSection) exports

printExportSection :: JSExport -> String
printExportSection { name, jsRequire, jsPath } =
  "exports." <> name <> " = require('" <> jsRequire <> "')" <> pathSection <> ";"
  where
    pathSection = Array.foldMap ("." <> _) jsPath
