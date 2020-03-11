module Antd.Codegen.Types
       ( AntModule
       , Prop(..)
       , PropTyp
       , requiredPropTyp
       , optionalPropTyp
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
    , propTyp :: PropTyp
    }

type PropTyp =
  { typ :: Typ
  , required :: Boolean
  }

requiredPropTyp :: Typ -> PropTyp
requiredPropTyp typ =
  { typ, required: true }

optionalPropTyp :: Typ -> PropTyp
optionalPropTyp typ =
  { typ, required: false }


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
          , input :: Array PropTyp
          , output :: PropTyp
          }
  | TypRecord (Array ( { key :: String
                       , propTyp :: PropTyp
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
    , declarations :: Array PSDecl
    }

data PSDeclName
  = PSDeclNameFun String
  | PSDeclNameType { name :: String, includeConstructors :: Boolean }
  | PSDeclNameClass String

type PSImport = { mod :: String, names :: Array PSDeclName }

type PSRecordRow =
  { name :: String
  , propTyp :: PropTyp
  , documentation :: Maybe String
  }

data PSDecl
  = PSDeclTypeRecord
    { name :: String
    , rows :: Array PSRecordRow
    }
  | PSDeclForeignRC
    { funName :: String
    , foreignComponentName :: String
    , propsName :: String
    }
