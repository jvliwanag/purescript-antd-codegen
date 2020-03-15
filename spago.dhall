{ name = "antd-codegen"
, dependencies =
  [ "console"
  , "effect"
  , "maybe"
  , "node-fs-aff"
  , "psci-support"
  , "spec"
  , "strings"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
