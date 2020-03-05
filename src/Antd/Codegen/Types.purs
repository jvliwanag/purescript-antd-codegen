module Antd.Codegen.Types
       ( AntModule
       , Prop(..)
       , Typ(..)
         -- purescript syntax
       , PSModule
       , PSDeclName(..)
       , PSImport
       ) where

import Data.Maybe (Maybe)


type AntModule
  = { name :: String
    , primaryProps :: Array Prop
    , subComponents ::
         Array { name :: String
               , props :: Array Prop
               }
    }

type Prop
  = { name :: String
    , description :: String
    , docType :: Maybe String
    , docDefault :: Maybe String
    , required :: Boolean
    , typ :: Typ
    }

data Typ
  = TypString
  | TypInt
  | TypNumber
  | TypBoolean
  | TypRef { name :: String }
  | TypUnknown
  | TypStringLit String
  | TypBooleanLit Boolean
  | TypOneOf (Array Typ)
  | TypArray Typ
  | TypFn { effectful :: Boolean
          , input :: Array Typ
          , output :: Typ
          }
  | TypRecord (Array ( { key :: String
                       , required :: Boolean
                       , typ :: Typ
                       }
                     ) )
  | TypNode
  | TypUnit


--

type PSModule
  = { name :: String
    , exports :: Array PSDeclName
    , importPrelude :: Boolean
    , imports :: Array PSImport
    }

data PSDeclName
  = PSDeclNameFun String
  | PSDeclNameType { name :: String, includeConstructors :: Boolean }
  | PSDeclNameClass String

type PSImport = { mod :: String, names :: Array PSDeclName }
