module Antd.Codegen
       ( run
       ) where

import Prelude

import Antd.Codegen.JSPrinter (printJSExports)
import Antd.Codegen.ModuleBundler (createModuleBundle)
import Antd.Codegen.PSPrinter (printModule)
import Antd.Codegen.Types (AntModule, JSExport, ModuleBundle, PSModule, Prop, Typ(..), optionalProp, optionalProp_)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdir, readdir, rmdir, unlink, writeTextFile)
import Node.Path (FilePath)
import Node.Path as Path

run :: Aff Unit
run = do
  let bundles = createModuleBundle <$> modules
  prepSrcDir
  for_ bundles saveModuleBundle
  log "Done!"

modules :: Array AntModule
modules =
  [ tableModule
  ]

generatedSrcDir :: FilePath
generatedSrcDir =
  Path.concat ["generated", "src", "generated"]

prepSrcDir :: Aff Unit
prepSrcDir = do
  clearDir
  mkdir generatedSrcDir

  where
    clearDir =
      whenM (exists generatedSrcDir)
      ( do
           fileNames <- readdir generatedSrcDir
           let paths = fileNames <#> \n -> Path.concat [ generatedSrcDir, n ]
           traverse_ unlink paths
           rmdir generatedSrcDir
      )

saveModuleBundle :: ModuleBundle -> Aff Unit
saveModuleBundle { name, psModule, jsExports } =
  savePSModule name psModule *> saveJSExports name jsExports

savePSModule :: String -> PSModule -> Aff Unit
savePSModule name m = do
  log $ "writing " <> path
  writeTextFile UTF8 path code
  where
    path = Path.concat [ generatedSrcDir, name <> ".purs" ]
    code = printModule m

saveJSExports :: String -> Array JSExport -> Aff Unit
saveJSExports name j = do
  log $ "writing " <> path
  writeTextFile UTF8 path code
  where
    path = Path.concat [ generatedSrcDir, name <> ".js" ]
    code = printJSExports j


tableModule :: AntModule
tableModule =
  { primaryComponent:
    { name: "Table"
    , props:
      [ optionalProp
        "tableLayout"
        (TypOneOf [ TypStringLit "auto", TypStringLit "fixed" ])
        (Just $ "[table-layout](https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout) attribute of table element"
          <> "\nType: - | `auto` | `fixed`"
          <> "\nDefault: -<hr />`fixed` when header/columns are fixed, or using `column.ellipsis`"
        )

      , optionalProp
        "bordered"
        TypBoolean
        (Just $ "Whether to show all table borders"
          <> "\nType: boolean"
          <> "\nDefault: `false`"
        )

      , optionalProp
        "columns"
        (TypArray (TypRef { name: "ColumnProps" }))
        (Just $ "Columns of table"
          <> "\nType: [ColumnProps](#Column)[]"
          <> "\nDefault: -"
        )

      , optionalProp
        "components"
        TypUnknown
        (Just $ "Override default table elements"
          <> "\nType: [TableComponents](https://git.io/fANxz)"
          <> "\nDefault: -"
        )

      , optionalProp
        "dataSource"
        (TypArray TypUnknown)
        (Just $ "Data record array to be displayed"
          <> "\nType: any[]"
          <> "\nDefault: -"
        )

      , optionalProp
        "expandable"
        (TypRef ({ name: "Expandable" }))
        (Just $ "Config expandable content"
          <> "\nType: [expandable](#expandable)"
          <> "\nDefault: -"
        )


      , optionalProp
        "footer"
        (TypFn { effectful: false
               , input: [ TypUnknown
                        ]
               , output: TypNode
               }
        )
        (Just $ "Table footer renderer"
          <> "\nType: Function(currentPageData)"
          <> "\nDefault: -")


      , optionalProp
        "loading"
        (TypOneOf [ TypBoolean ])
        (Just $ "Loading status of table"
          <> "\nType: boolean|[object](https://ant.design/components/spin-cn/#API) ([more](https://github.com/ant-design/ant-design/issues/4544#issuecomment-271533135))"
          <> "\nDefault: `false`"
        )


      , optionalProp
        "locale"
        (TypRecord
          [ optionalProp_ "filterConfirm" TypString
          , optionalProp_ "filterReset" TypString
          , optionalProp_ "emptyText" TypString
          ]
        )
        (Just $ "i18n text including filter, sort, empty text, etc"
          <> "\nType: object"
          <> "\nDefault: filterConfirm: 'Ok' <br> filterReset: 'Reset' <br> emptyText: 'No Data' <br> [Default](https: //github.com/ant-design/ant-design/issues/575#issuecomment-159169511)")


      , optionalProp
        "pagination"
        TypUnknown
        (Just $ "Config of pagination. You can ref table pagination [config](#pagination) or full [`pagination`](/components/pagination/) document, hide it by setting it to `false`"
          <> "\nType: object"
          <> "\nDefault: -")

      , optionalProp
        "rowClassName"
        (TypUnknown)
        (Just $ "Row's className"
          <> "\nType: Function(record, index):string"
          <> "\nDefault: -")

      , optionalProp
        "rowKey"
        (TypString)
        (Just $ "Row's unique key, could be a string or function that returns a string"
          <> "\nType: string|Function(record):string"
          <> "\nDefault: `key`")

      , optionalProp
        "rowSelection"
        (TypRef { name: "RowSelection" })
        (Just $ "Row selection [config](#rowSelection)"
          <> "\nType: object"
          <> "\nDefault: null")

      , optionalProp
        "scroll"
        (TypRef { name: "Scroll" })
        (Just $ "Whether the table can be scrollable, [config](#scroll)"
          <> "\nType: object"
          <> "\nDefault: -")

      , optionalProp
        "showHeader"
        (TypBoolean)
        (Just $ "Whether to show table header"
          <> "\nType: boolean"
          <> "\nDefault: `true`")

      , optionalProp
        "size"
        (TypOneOf [ TypStringLit "default"
                  , TypStringLit "middle"
                  , TypStringLit "small"
                  ])
        (Just $ "Size of table"
          <> "\nType: `default` | `middle` | `small`"
          <> "\nDefault: `default`")

      , optionalProp
        "summary"
        (TypFn { effectful: false
                     , input: [ TypUnknown ]
                     , output: TypNode
                     }
        )
        (Just $ "Summary content"
          <> "\nType: (currentData) => ReactNode"
          <> "\nDefault: -")


      , optionalProp
        "title"
        (TypFn { effectful: false
                     , input: [ TypUnknown
                              ]
                     , output: TypNode
                     }
        )
        (Just $ "Table title renderer"
          <> "\nType: Function(currentPageData)"
          <> "\nDefault: -")

      , optionalProp
        "onChange"
        (TypFn { effectful: true
                     , input: [ TypUnknown
                              , TypUnknown
                              , TypUnknown
                              , TypUnknown
                              ]
                     , output: TypUnit
                     }
        )
        (Just $ "Callback executed when pagination, filters or sorter is changed"
          <> "\nType: Function(pagination, filters, sorter, extra: { currentDataSource: [] })")


      , optionalProp
        "onHeaderRow"
        (TypFn { effectful: true
               , input: [ TypUnknown
                        , TypInt
                        , TypUnit
                        ]
               , output: TypUnit
               }
        )
        (Just $ "Set props on per header row"
          <> "\nType: Function(column, index)"
          <> "\nDefault: -")


      , optionalProp
        "onRow"
        (TypFn { effectful: true
               , input: [ TypUnknown
                        , TypInt
                        ]
               , output: TypUnit
               }
        )
        (Just $ "Set props on per row"
          <> "\nType: Function(record, index)"
          <> "\nDefault: -")


      , optionalProp
        "getPopupContainer"
        (TypFn { effectful: false
                     , input: [ TypUnknown
                              ]
                     , output: TypUnknown
                     })
        (Just $ "the render container of dropdowns in table"
          <> "\nType: (triggerNode) => HTMLElement"
          <> "\nDefault: `() => TableHtmlElement`")


      , optionalProp
        "sortDirections"
        (TypArray (TypOneOf [ TypStringLit "ascend", TypStringLit "descend" ]))
        (Just $ "supported sort way, could be `'ascend'`, `'descend'`"
          <> "\nType: Array"
          <> "\nDefault: `['ascend', 'descend']`"
        )
      ]
    }
  , subComponents:
    [
    ]
  }

columnPropsDef :: Array Prop
columnPropsDef =
  [ optionalProp
    "align"
    (TypOneOf [ TypStringLit "left",  TypStringLit "right", TypStringLit "center" ])
    (Just $ "specify which way that column is aligned"
      <> "\nType: `left` | `right` | `center`"
      <> "\nDefault: `left`"
    )

  , optionalProp
    "ellipsis"
    TypBoolean
    (Just $ "ellipsis cell content, not working with sorter and filters for now.<br />tableLayout would be `fixed` when `ellipsis` is true."
      <> "\nType: boolean"
      <> "\nDefault: false")

  , optionalProp
    "className"
    TypString
    (Just $ "className of this column"
      <> "\nType: string"
      <> "\nDefault: -")

  , optionalProp
    "colSpan"
    TypNumber
    (Just $ "Span of this column's title"
      <> "\nType: number"
      <> "\nDefault: -")

  , optionalProp
    "dataIndex"
    (TypOneOf [ TypString, TypArray TypString ])
    (Just $ "Display field of the data record, support nest path by string array"
      <> "\nType: string | string[]"
      <> "\nDefault: -")

  , optionalProp
    "defaultFilteredValue"
    (TypArray TypString)
    (Just $ "Default filtered values"
      <> "\nType: string | []"
      <> "\nDefault: - |")

  , optionalProp
    "defaultSortOrder"
    (TypOneOf [ TypStringLit "ascend", TypStringLit "descend" ])
    (Just $ "Default order of sorted values"
      <> "\nType: `ascend` | `descend`"
      <> "\nDefault: -")

  , optionalProp
    "filterDropdown"
    (TypOneOf [ TypNode
              , TypFn { effectful: false
                      , input: [ TypUnknown
                               ]
                      , output: TypNode
                      }
              ])
    (Just $ "Customized filter overlay"
      <> "\nType: React.ReactNode | (props: [FilterDropdownProps](https://git.io/fjP5h)) => React.ReactNode"
      <> "\nDefault: -"
    )

  , optionalProp
    "filterDropdownVisible"
    (TypBoolean)
    (Just $ "Whether `filterDropdown` is visible"
      <> "\nType: boolean"
      <> "\nDefault: -"
    )

  , optionalProp
    "filtered"
    (TypBoolean)
    (Just $ "Whether the `dataSource` is filtered"
      <> "\nType: boolean"
      <> "\nDefault: `false`"
    )

  , optionalProp
    "filteredValue"
    (TypArray TypString)
    (Just $ "Controlled filtered value, filter icon will highlight"
      <> "\nType: string[]"
      <> "\nDefault: -"
    )

  , optionalProp
    "filterIcon"
    (TypOneOf [ TypNode
              , TypFn { effectful: false
                      , input: [ TypBoolean
                               ]
                      , output: TypNode
                      }
              ])
    (Just $ "Customized filter icon"
     <> "\nType: ReactNode|(filtered: boolean) => ReactNode"
     <> "\nDefault: `false`")

  , optionalProp
    "filterMultiple"
    (TypBoolean)
    (Just $ "Whether multiple filters can be selected"
     <> "\nType: boolean"
     <> "\nDefault: `true`")

  , optionalProp
    "filters"
    (TypArray TypUnknown)
    (Just $ "Filter menu config"
     <> "\nType: object[]"
     <> "\nDefault: -")

  , optionalProp
    "fixed"
    (TypOneOf [ TypBoolean
                    , TypString
                    ]
      )
    (Just $ "(\"IE not support) Set column to be fixed: `true`(same as left) `'left'` `'right'`"
      <> "\nType: boolean|string")

  , optionalProp
    "key"
    TypString
    (Just $ "Unique key of this column, you can ignore this prop if you've set a unique `dataIndex`"
      <> "\nType: string"
      <> "\nDefault: -"
    )

  , optionalProp
    "render"
    (TypFn { effectful: false
           , input: [ TypString
                    , TypUnknown
                    , TypInt
                    , TypNode
                    ]
           , output: TypUnit
           })
    (Just $"Renderer of the table cell. The return value should be a ReactNode, or an object for [colSpan/rowSpan config](#components-table-demo-colspan-rowspan)")


  , optionalProp
    "sorter"
    (TypOneOf [ TypBoolean
              , TypFn { effectful: false
                      , input: []
                      , output: TypUnknown
                      }
              ]

    )
    (Just $ "Sort function for local sort, see [Array.sort](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort)'s compareFunction. If you need sort buttons only, set to `true`")


  , optionalProp
    "sortOrder"
    (TypOneOf [ TypBooleanLit false
              , TypStringLit "ascend"
              , TypStringLit "descend"
              ]
    )
    (Just $ "Order of sorted values: `'ascend'` `'descend'` `false`"
      <> "\nType: boolean|string"
      <> "\nDefault: -")


  , optionalProp
    "sortDirections"
    (TypArray ( TypOneOf [ TypStringLit "ascend"
                         , TypStringLit "descend"
                         ]
              )
    )
    (Just $ "supported sort way, override `sortDirections` in `Table`, could be `'ascend'`, `'descend'`"
      <> "\nType: Array")


  , optionalProp
    "title"
    (TypOneOf [ TypNode
              , TypFn { effectful: false
                      , input:
                        [ TypRecord [ optionalProp_ "sortOrder"  TypInt
                                    , optionalProp_ "sortColumn" TypUnknown
                                    , optionalProp_ "filters" TypUnknown
                                    ]
                        ]
                      , output: TypNode
                      }
              ])
    (Just $ "Title of this column"
      <> "\nType: ReactNode|({ sortOrder, sortColumn, filters }) => ReactNode"
      <> "\nDefault: -")


  , optionalProp
    "width"
    (TypOneOf [ TypString, TypNumber ])
    (Just $ "Width of this column ([width not working?](https://github.com/ant-design/ant-design/issues/13825#issuecomment-449889241))"
      <> "\nType: string|number")

  , optionalProp
    "onCell"
    (TypFn { effectful: true
           , input: [ TypUnknown
                    , TypInt
                    ]
           , output: TypUnit
           })
    (Just $ "Set props on per cell"
      <> "\nType: Function(record, rowIndex)"
      <> "\nDefault: -")


  , optionalProp
    "onFilter"
    (TypFn { effectful: true
           , input: []
           , output: TypUnit
           }
    )
    (Just $ "Callback executed when the confirm filter button is clicked"
      <> "\nType: Function"
      <> "\nDefault: -")


  , optionalProp
    "onFilterDropdownVisibleChange"
    (TypFn { effectful: true
           , input: [ TypBoolean
                    ]
           , output: TypUnit
           }
    )
    (Just $ "Callback executed when `filterDropdownVisible` is changed"
      <> "\nType: function(visible) {}"
      <> "\nDefault: -")


  , optionalProp
    "onHeaderCell"
    (TypFn { effectful: true
           , input: [ TypUnknown
                    ]
           , output: TypUnit
           })
    (Just $"Set props on per header cell"
      <> "\nType: Function(column)"
      <> "\nDefault: -")

  ]
