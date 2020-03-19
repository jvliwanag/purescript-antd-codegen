module Antd.Codegen.Types
       ( AntModule
       , Component
       , Prop
       , prop
       , prop_
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
       , PSTypeDecl(..)
       , psTypeDecl_
       , psTypeDecl
       , psTypeDecl'
       , psTypeDeclOp
       , psTypeDeclOp'
       , psTypeDeclRecord
       , PSTypeArg(..)
       , psTypeArg
       , psTypeArgSymbol
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
    , propTyp :: PropTyp
    , doc :: Maybe String
    }

prop :: String -> PropTyp -> Maybe String -> Prop
prop name propTyp doc = { name, propTyp, doc }

prop_ :: String -> PropTyp -> Prop
prop_ name propTyp = prop name propTyp Nothing

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
  | TypRecord (Array Prop)

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
  , typeDecl :: PSTypeDecl
  , doc :: Maybe String
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

data PSTypeDecl
  = PSTypeDeclCons { consName :: String, args :: Array PSTypeArg }
  | PSTypeDeclOp { symbol :: String, args :: Array PSTypeArg }
  | PSTypeDeclRecord { fields :: Array { name :: String, typeDecl :: PSTypeDecl } }

derive instance psTypeDeclEq :: Eq PSTypeDecl
derive instance psTypeDeclGeneric :: Generic PSTypeDecl _

instance psTypeDeclShow :: Show PSTypeDecl where
  show d = genericShow d

psTypeDecl_ :: String -> PSTypeDecl
psTypeDecl_ consName = psTypeDecl consName []

psTypeDecl :: String -> Array PSTypeDecl -> PSTypeDecl
psTypeDecl consName = psTypeDecl' consName <<< map psTypeArg

psTypeDecl' :: String -> Array PSTypeArg -> PSTypeDecl
psTypeDecl' consName args = PSTypeDeclCons { consName, args }

psTypeDeclOp :: String -> Array PSTypeDecl -> PSTypeDecl
psTypeDeclOp symbol = psTypeDeclOp' symbol <<< map psTypeArg

psTypeDeclOp' :: String -> Array PSTypeArg -> PSTypeDecl
psTypeDeclOp' symbol args = PSTypeDeclOp { symbol, args }


psTypeDeclRecord :: Array { name :: String, typeDecl :: PSTypeDecl } -> PSTypeDecl
psTypeDeclRecord fields = PSTypeDeclRecord { fields }

data PSTypeArg
  = PSTypeArgDecl PSTypeDecl
  | PSTypeArgSymbol String

derive instance psTypeArgEq :: Eq PSTypeArg
derive instance psTypeArgGeneric :: Generic PSTypeArg _

instance psTypeArgShow :: Show PSTypeArg where
  show = genericShow

psTypeArg :: PSTypeDecl -> PSTypeArg
psTypeArg = PSTypeArgDecl

psTypeArgSymbol :: String -> PSTypeArg
psTypeArgSymbol = PSTypeArgSymbol

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
