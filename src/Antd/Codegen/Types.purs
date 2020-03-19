module Antd.Codegen.Types
       ( AntModule
       , Component
       , Prop
       , prop
       , prop_
       , optionalProp
       , optionalProp_
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
    , typ :: Typ
    , doc :: Maybe String
    }

prop :: String -> Typ -> Maybe String -> Prop
prop name typ doc = { name, typ, doc }

prop_ :: String -> Typ -> Prop
prop_ name typ = prop name typ Nothing

optionalProp :: String -> Typ -> Maybe String -> Prop
optionalProp name typ doc = { name, typ: TypUndefinedOr typ, doc }

optionalProp_ :: String -> Typ -> Prop
optionalProp_ name typ = optionalProp name typ Nothing


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

  | TypUndefinedOr Typ
  | TypOneOf (Array Typ)
  | TypArray Typ
  | TypFn { effectful :: Boolean
          , input :: Array Typ
          , output :: Typ
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

type JSExport
  = { name :: String
    , jsRequire :: String
    , jsPath :: Array String
    }

jsExport :: String -> String -> Array String -> JSExport
jsExport name jsRequire jsPath = { name, jsRequire, jsPath }

--

type ModuleBundle
  = { name :: String
    , psModule :: PSModule
    , jsExports :: Array JSExport
    }
