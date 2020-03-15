module Main where

import Prelude

import Antd.Codegen as Codegen
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ Codegen.run
