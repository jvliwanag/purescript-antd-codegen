module Antd.Codegen.ModuleBundler
       ( createPSModule
       ) where

import Prelude

import Antd.Codegen.Types (AntModule, PSDecl(..), PSDeclName(..), PSModule)
import Data.Maybe (Maybe(..))
import Data.String as String

createPSModule :: AntModule -> PSModule
createPSModule { name } =
  { name: "Antd." <> name
  , exports:
    [ PSDeclNameType { name: pcn.propsName
                     , includeConstructors: false
                     }
    , PSDeclNameFun pcn.funName
    ]
  , importPrelude: false
  , imports:
    [ { mod: "React.Basic"
      , names: [ PSDeclNameType { includeConstructors: false
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
    ]
  , declarations:
    [ PSDeclTypeRecord
              { name: pcn.propsName
              , rows: []
              }
    , PSDeclForeignRC pcn
    ]
  }

  where
    pcn = mkCompNames name

type CompNames =
  { funName :: String
  , propsName :: String
  , foreignComponentName :: String
  }

mkCompNames :: String -> CompNames
mkCompNames baseName =
  { funName: decapName
  , propsName: baseName <> "Props"
  , foreignComponentName: "_" <> decapName
  }

  where
    decapName = decapitalize baseName

decapitalize :: String -> String
decapitalize s = case String.uncons s of
  Just { head, tail } -> String.toLower (String.singleton head) <> tail
  Nothing -> s
