module Antd.Codegen.JSPrinter
       ( printJSBinding
       ) where

import Prelude

import Antd.Codegen.Types (JSBinding, JSExport)
import Data.Array as Array
import Data.Maybe (Maybe(..))

printJSBinding :: JSBinding -> String
printJSBinding { antSubmodule, exports } =
  printSubmoduleSection antSubmodule
  <> "\n"
  <> "\n" <> printExportsSection antSubmodule exports
  <> "\n"

printSubmoduleSection :: String -> String
printSubmoduleSection antSubmodule =
  "const " <> antSubmodule <> " = require('antd')." <> antSubmodule <> ";"

printExportsSection :: String -> Array JSExport -> String
printExportsSection antSubmodule exports =
  Array.intercalate "\n" $ printExportSection antSubmodule <$> exports

printExportSection :: String -> JSExport -> String
printExportSection antSubmodule { name, member } =
  "exports." <> name <> " = " <> antSubmodule <> memberSection <> ";"
  where
    memberSection = case member of
      Just m -> "." <> m
      Nothing -> ""
