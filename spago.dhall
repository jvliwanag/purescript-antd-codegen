{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "antd"

, dependencies =
  [ "console"
  , "effect"
  , "literal"
  , "oneof"
  , "psci-support"
  , "react-basic"
  ]
  
, packages = ./packages.dhall

, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
