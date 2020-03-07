module Antd.Codegen.Types
       ( AntModule
       , Prop(..)
       , Typ(..)
         -- purescript syntax
       , PSModule
       , PSDeclName(..)
       , PSImport
       , PSRecordRow
       , PSDecl(..)
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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
  | TypNode
  | TypUnit

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

derive instance typGeneric :: Generic Typ _

instance typShow :: Show Typ where
  show t = genericShow t

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

type PSRecordRow =
  { name :: String
  , allowUndefined :: Boolean
  , typ :: String
  , documentation :: Maybe String
  }

data PSDecl
  = PSDeclTypeRecord
    { name :: String
    , rows :: Array PSRecordRow
    }
