module Antd.Codegen.Types
       ( AntModule
       , Component
       , Prop
       , prop
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
         -- js syntax
       , JSBinding
       , JSExport
       , jsExport
         -- bundle
       , ModuleBundle
       ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))


type AntModule
  = { primaryComponent :: Component
    , subComponents :: Array Component
    }

type Component =
  { name :: String
  , props :: Array Prop
  }

type Prop
  = { name :: String
    , docDescription :: Maybe String
    , docType :: Maybe String
    , docDefault :: Maybe String
    , propTyp :: PropTyp
    }

prop :: String -> PropTyp -> Prop
prop name propTyp =
  { name
  , propTyp
  , docDescription: Nothing
  , docType: Nothing
  , docDefault: Nothing
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

derive instance typEq :: Eq Typ
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
  = PSDeclNameClass String
  | PSDeclNameType { name :: String, includeConstructors :: Boolean }
  | PSDeclNameFun String

derive instance psDeclNameEq :: Eq PSDeclName
derive instance psDeclNameOrd :: Ord PSDeclName
derive instance psDeclNameGeneric :: Generic PSDeclName _

instance psDeclNameShow :: Show PSDeclName where
  show = genericShow

type PSImport = { mod :: String, names :: Array PSDeclName }

type PSRecordRow =
  { name :: String
  , typ :: String
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

derive instance psDeclEq :: Eq PSDecl
derive instance psDeclGeneric :: Generic PSDecl _

instance psDeclShow :: Show PSDecl where
  show = genericShow

--

type JSBinding
  = { antSubmodule :: String
    , exports :: Array JSExport
    }

type JSExport
  = { name :: String
    , member :: Maybe String
    }

jsExport :: String -> Maybe String -> JSExport
jsExport name member = { name, member }

--

type ModuleBundle
  = { name :: String
    , psModule :: PSModule
    , jsBinding :: JSBinding
    }
