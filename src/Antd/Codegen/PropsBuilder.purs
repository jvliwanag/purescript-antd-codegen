module Antd.Codegen.PropsBuilder
       ( PropsBuilder
       , buildProps
       , addProp
       , addRequiredProp
       , addOptionalProp
       ) where

import Prelude

import Antd.Codegen.Types (Prop, Typ)
import Control.Monad.State (State, execState, modify_)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))

type PropsBuilder a = State (List Prop) a

buildProps :: PropsBuilder Unit -> Array Prop
buildProps =
  Array.fromFoldable <<< List.reverse <<< (flip execState) mempty

addProp :: String -> Boolean -> Typ -> PropsBuilder Unit
addProp name required typ =
  modify_ (List.Cons { name
                     , doc: Nothing
                     , propTyp:
                       { required
                       , typ
                       }
                     })

addRequiredProp :: String -> Typ -> PropsBuilder Unit
addRequiredProp name typ =
  addProp name true typ

addOptionalProp :: String -> Typ -> PropsBuilder Unit
addOptionalProp name typ =
  addProp name true typ
