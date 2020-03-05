module Antd.Codegen.Printer
       ( printModuleSection
       ) where

import Prelude

import Antd.Codegen.Types (PSDeclName(..), PSModule)
import Data.Array as Array

printModuleSection :: PSModule -> String
printModuleSection { name, exports } =
  "module " <> name
  <> "\n" <> importSection

  where
    importSection = case exports of
      [] -> "  where"
      _ ->
        "  ( "
        <> (Array.intercalate "\n  , " $ printDeclName <$> exports)
        <> "\n  ) where"

printDeclName :: PSDeclName -> String
printDeclName (PSDeclNameFun n) = n
printDeclName (PSDeclNameType { name, includeConstructors }) =
  if includeConstructors
  then name <> "(..)"
  else name
printDeclName (PSDeclNameClass n) =
  "class " <> n
