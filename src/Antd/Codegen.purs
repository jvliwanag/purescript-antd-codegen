module Antd.Codegen
       ( run
       ) where

import Prelude

import Antd.Codegen.JSPrinter (printJSBinding)
import Antd.Codegen.ModuleBundler (createModuleBundle)
import Antd.Codegen.PSPrinter (printModule)
import Antd.Codegen.Types (AntModule, ModuleBundle, PSModule, Prop, Typ(..), JSBinding, requiredPropTyp)
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
saveModuleBundle { name, psModule, jsBinding } =
  savePSModule name psModule *> saveJSBinding name jsBinding

savePSModule :: String -> PSModule -> Aff Unit
savePSModule name m = do
  log $ "writing " <> path
  writeTextFile UTF8 path code
  where
    path = Path.concat [ generatedSrcDir, name <> ".purs" ]
    code = printModule m

saveJSBinding :: String -> JSBinding -> Aff Unit
saveJSBinding name j = do
  log $ "writing " <> path
  writeTextFile UTF8 path code
  where
    path = Path.concat [ generatedSrcDir, j.antSubmodule <> ".js" ]
    code = printJSBinding j


tableModule :: AntModule
tableModule =
  { primaryComponent:
    { name: "Table"
    , props:
      [ { name: "tableLayout"
        , doc: Just $ "[table-layout](https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout) attribute of table element"
          <> "\nType: - | `auto` | `fixed`"
          <> "\nDefault: -<hr />`fixed` when header/columns are fixed, or using `column.ellipsis`"
        , propTyp:
        { required: false
        , typ: TypOneOf [ TypStringLit "auto", TypStringLit "fixed" ]
        }
        }

      , { name: "bordered"
        , doc: Just $ "Whether to show all table borders"
          <> "\nType: boolean"
          <> "\nDefault: `false`"
        , propTyp:
        { required: false
        , typ: TypBoolean
        }
        }

      , { name: "columns"
        , doc: Just $ "Columns of table"
          <> "\nType: [ColumnProps](#Column)[]"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypArray (TypRef { name: "ColumnProps" })
        }
        }

      , { name: "components"
        , doc: Just $ "Override default table elements"
          <> "\nType: [TableComponents](https://git.io/fANxz)"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypUnknown
        }
        }

      , { name: "dataSource"
        , doc: Just $ "Data record array to be displayed"
          <> "\nType: any[]"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypArray TypUnknown
        }
        }

      , { name: "expandable"
        , doc: Just $ "Config expandable content"
          <> "\nType: [expandable](#expandable)"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypRef ({ name: "Expandable" })
        }
        }

      , { name: "footer"
        , doc: Just $ "Table footer renderer"
          <> "\nType: Function(currentPageData)"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypFn { effectful: false
                     , input: [ requiredPropTyp TypUnknown
                              ]
                     , output: requiredPropTyp TypNode
                     }
        }
        }

      , { name: "loading"
        , doc: Just $ "Loading status of table"
          <> "\nType: boolean|[object](https://ant.design/components/spin-cn/#API) ([more](https://github.com/ant-design/ant-design/issues/4544#issuecomment-271533135))"
          <> "\nDefault: `false`"
        , propTyp:
        { required: false
        , typ: TypOneOf [ TypBoolean ]
        }
        }

      , { name: "locale"
        , doc: Just $ "i18n text including filter, sort, empty text, etc"
          <> "\nType: object"
          <> "\nDefault: filterConfirm: 'Ok' <br> filterReset: 'Reset' <br> emptyText: 'No Data' <br> [Default](https: //github.com/ant-design/ant-design/issues/575#issuecomment-159169511)"
        , propTyp:
        { required: false
        , typ: TypRecord
          [ { key: "filterConfirm"
            , propTyp:
              { required: false
              , typ: TypString
              }
            }
          , { key: "filterReset"
            , propTyp:
              { required: false
              , typ: TypString
              }
            }
          , { key: "emptyText"
            , propTyp:
              { required: false
              , typ: TypString
              }
            }
          ]
        }
        }

      , { name: "pagination"
        , doc: Just $ "Config of pagination. You can ref table pagination [config](#pagination) or full [`pagination`](/components/pagination/) document, hide it by setting it to `false`"
          <> "\nType: object"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypUnknown
        }
        }

      , { name: "rowClassName"
        , doc: Just $ "Row's className"
          <> "\nType: Function(record, index):string"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypUnknown
        }
        }

      , { name: "rowKey"
        , doc: Just $ "Row's unique key, could be a string or function that returns a string"
          <> "\nType: string|Function(record):string"
          <> "\nDefault: `key`"
        , propTyp:
        { required: false
        , typ: TypString
        }
        }

      , { name: "rowSelection"
        , doc: Just $ "Row selection [config](#rowSelection)"
          <> "\nType: object"
          <> "\nDefault: null"
        , propTyp:
        { required: false
        , typ: TypRef { name: "RowSelection" }
        }
        }

      , { name: "scroll"
        , doc: Just $ "Whether the table can be scrollable, [config](#scroll)"
          <> "\nType: object"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypRef { name: "Scroll" }
        }
        }

      , { name: "showHeader"
        , doc: Just $ "Whether to show table header"
          <> "\nType: boolean"
          <> "\nDefault: `true`"
        , propTyp:
        { required: false
        , typ: TypBoolean
        }
        }

      , { name: "size"
        , doc: Just $ "Size of table"
          <> "\nType: `default` | `middle` | `small`"
          <> "\nDefault: `default`"
        , propTyp:
        { required: false
        , typ: TypOneOf [ TypStringLit "default"
                        , TypStringLit "middle"
                        , TypStringLit "small"
                        ]
        }
        }

      , { name: "summary"
        , doc: Just $ "Summary content"
          <> "\nType: (currentData) => ReactNode"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypFn { effectful: false
                     , input: [ requiredPropTyp TypUnknown ]
                     , output: requiredPropTyp TypNode
                     }
        }
        }

      , { name: "title"
        , doc: Just $ "Table title renderer"
          <> "\nType: Function(currentPageData)"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypFn { effectful: false
                     , input: [ requiredPropTyp TypUnknown
                              ]
                     , output: requiredPropTyp TypNode
                     }
        }
        }

      , { name: "onChange"
        , doc: Just $ "Callback executed when pagination, filters or sorter is changed"
          <> "\nType: Function(pagination, filters, sorter, extra: { currentDataSource: [] })"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypFn { effectful: true
                     , input: [ requiredPropTyp TypUnknown
                              , requiredPropTyp TypUnknown
                              , requiredPropTyp TypUnknown
                              , requiredPropTyp TypUnknown
                              ]
                     , output: requiredPropTyp TypUnit
                     }
        }
        }

      , { name: "onHeaderRow"
        , doc: Just $ "Set props on per header row"
          <> "\nType: Function(column, index)"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypFn { effectful: true
                     , input: [ requiredPropTyp TypUnknown
                              , requiredPropTyp TypInt
                              , requiredPropTyp TypUnit
                              ]
                     , output: requiredPropTyp TypUnit
                     }
        }
        }

      , { name: "onRow"
        , doc: Just $ "Set props on per row"
          <> "\nType: Function(record, index)"
          <> "\nDefault: -"
        , propTyp:
        { required: false
        , typ: TypFn { effectful: true
                     , input: [ requiredPropTyp TypUnknown
                              , requiredPropTyp TypInt
                              ]
                     , output: requiredPropTyp TypUnit
                     }
        }
        }

      , { name: "getPopupContainer"
        , doc: Just $ "the render container of dropdowns in table"
          <> "\nType: (triggerNode) => HTMLElement"
          <> "\nDefault: `() => TableHtmlElement`"
        , propTyp:
        { required: false
        , typ: TypFn { effectful: false
                     , input: [ requiredPropTyp TypUnknown
                              ]
                     , output: requiredPropTyp TypUnknown
                     }
        }
        }

      , { name: "sortDirections"
        , doc: Just $ "supported sort way, could be `'ascend'`, `'descend'`"
          <> "\nType: Array"
          <> "\nDefault: `['ascend', 'descend']`"
        , propTyp:
        { required: false
        , typ: TypArray (TypOneOf [ TypStringLit "ascend", TypStringLit "descend" ])
        }
        }
      ]
    }
  , subComponents:
    [
    ]
  }

columnPropsDef :: Array Prop
columnPropsDef =
  [ { name: "align"
    , doc: Just $ "specify which way that column is aligned"
      <> "\nType: `left` | `right` | `center`"
      <> "\nDefault: `left`"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypStringLit "left",  TypStringLit "right", TypStringLit "center" ]
      }
    }

  , { name: "ellipsis"
    , doc: Just $ "ellipsis cell content, not working with sorter and filters for now.<br />tableLayout would be `fixed` when `ellipsis` is true."
      <> "\nType: boolean"
      <> "\nDefault: false"
    , propTyp:
      { required: false
      , typ: TypBoolean
      }
    }

  , { name: "className"
    , doc: Just $ "className of this column"
      <> "\nType: string"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypString
      }
    }

  , { name: "colSpan"
    , doc: Just $ "Span of this column's title"
      <> "\nType: number"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypNumber
      }
    }

  , { name: "dataIndex"
    , doc: Just $ "Display field of the data record, support nest path by string array"
      <> "\nType: string | string[]"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypString, TypArray TypString ]
      }
    }

  , { name: "defaultFilteredValue"
    , doc: Just $ "Default filtered values"
      <> "\nType: string | []"
      <> "\nDefault: - |"
    , propTyp:
      { required: false
      , typ: TypArray TypString
      }
    }

  , { name: "defaultSortOrder"
    , doc: Just $ "Default order of sorted values"
      <> "\nType: `ascend` | `descend`"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypStringLit "ascend", TypStringLit "descend" ]
      }
    }

  , { name: "filterDropdown"
    , doc: Just $ "Customized filter overlay"
      <> "\nType: React.ReactNode | (props: [FilterDropdownProps](https://git.io/fjP5h)) => React.ReactNode"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypNode
                    , TypFn { effectful: false
                            , input: [ requiredPropTyp TypUnknown
                                     ]
                            , output: requiredPropTyp TypNode
                            }
                    ]

      }
    }

  , { name: "filterDropdownVisible"
    , doc: Just $ "Whether `filterDropdown` is visible"
      <> "\nType: boolean"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypBoolean
      }
    }

  , { name: "filtered"
    , doc: Just $ "Whether the `dataSource` is filtered"
      <> "\nType: boolean"
      <> "\nDefault: `false`"
    , propTyp:
      { required: false
      , typ: TypBoolean
      }
    }

  , { name: "filteredValue"
    , doc: Just $ "Controlled filtered value, filter icon will highlight"
      <> "\nType: string[]"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypArray TypString
      }
    }

  , { name: "filterIcon"
    , doc: Just $ "Customized filter icon"
      <> "\nType: ReactNode|(filtered: boolean) => ReactNode"
      <> "\nDefault: `false`"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypNode
                      , TypFn { effectful: false
                              , input: [ requiredPropTyp TypBoolean
                                       ]
                              , output: requiredPropTyp TypNode
                              }
                      ]
      }
    }

  , { name: "filterMultiple"
    , doc: Just $ "Whether multiple filters can be selected"
      <> "\nType: boolean"
      <> "\nDefault: `true`"
    , propTyp:
      { required: false
      , typ: TypBoolean
      }
    }

  , { name: "filters"
    , doc: Just $ "Filter menu config"
      <> "\nType: object[]"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypArray TypUnknown
      }
    }

  , { name: "fixed"
    , doc: Just $ "(\"IE not support) Set column to be fixed: `true`(same as left) `'left'` `'right'`"
      <> "\nType: boolean|string"
      <> "\nDefault: `false`"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypBoolean
                    , TypString
                    ]
      }
    }

  , { name: "key"
    , doc: Just $ "Unique key of this column, you can ignore this prop if you've set a unique `dataIndex`"
      <> "\nType: string"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypString
      }
    }

  , { name: "render"
    , doc: Just $"Renderer of the table cell. The return value should be a ReactNode, or an object for [colSpan/rowSpan config](#components-table-demo-colspan-rowspan)"
      <> "\nType: Function(text, record, index) {}"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypFn { effectful: false
                 , input: [ requiredPropTyp TypString
                          , requiredPropTyp TypUnknown
                          , requiredPropTyp TypInt
                          , requiredPropTyp TypNode
                          ]
                 , output: requiredPropTyp TypUnit
                 }
      }
    }

  , { name: "sorter"
    , doc: Just $ "Sort function for local sort, see [Array.sort](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort)'s compareFunction. If you need sort buttons only, set to `true`"
      <> "\nType: Function|boolean"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypBoolean
                    , TypFn { effectful: false
                            , input: []
                            , output: requiredPropTyp TypUnknown
                            }
                    ]

      }
    }

  , { name: "sortOrder"
    , doc: Just $ "Order of sorted values: `'ascend'` `'descend'` `false`"
      <> "\nType: boolean|string"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypBooleanLit false
                    , TypStringLit "ascend"
                    , TypStringLit "descend"
                    ]
      }
    }

  , { name: "sortDirections"
    , doc: Just $ "supported sort way, override `sortDirections` in `Table`, could be `'ascend'`, `'descend'`"
      <> "\nType: Array"
      <> "\nDefault: `['ascend', 'descend']`"
    , propTyp:
      { required: false
      , typ: TypArray ( TypOneOf [ TypStringLit "ascend"
                               , TypStringLit "descend"
                               ]
                    )
      }
    }

  , { name: "title"
    , doc: Just $ "Title of this column"
      <> "\nType: ReactNode|({ sortOrder, sortColumn, filters }) => ReactNode"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypNode
                    , TypFn { effectful: false
                            , input: [ requiredPropTyp $
                                       TypRecord [ { key: "sortOrder"
                                                   , propTyp:
                                                     { required: false
                                                     , typ: TypInt
                                                     }
                                                   }
                                                 , { key: "sortColumn"
                                                   , propTyp:
                                                     { required: false
                                                     , typ: TypUnknown
                                                     }
                                                   }
                                                 , { key: "filters"
                                                   , propTyp:
                                                     { required: false
                                                     , typ: TypUnknown
                                                     }
                                                   }
                                                 ]
                                     ]
                            , output: requiredPropTyp TypNode
                            }
                    ]
      }
    }

  , { name: "width"
    , doc: Just $ "Width of this column ([width not working?](https://github.com/ant-design/ant-design/issues/13825#issuecomment-449889241))"
      <> "\nType: string|number"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypString, TypNumber ]
      }
    }

  , { name: "onCell"
    , doc: Just $ "Set props on per cell"
      <> "\nType: Function(record, rowIndex)"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypFn { effectful: true
                   , input: [ requiredPropTyp TypUnknown
                            , requiredPropTyp TypInt
                            ]
                   , output: requiredPropTyp TypUnit
                   }
      }
    }

  , { name: "onFilter"
    , doc: Just $ "Callback executed when the confirm filter button is clicked"
      <> "\nType: Function"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypFn { effectful: true
                   , input: []
                   , output: requiredPropTyp TypUnit
                   }
      }
    }

  , { name: "onFilterDropdownVisibleChange"
    , doc: Just $ "Callback executed when `filterDropdownVisible` is changed"
      <> "\nType: function(visible) {}"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypFn { effectful: true
                 , input: [ requiredPropTyp TypBoolean
                          ]
                 , output: requiredPropTyp TypUnit
                 }
      }
    }

  , { name: "onHeaderCell"
    , doc: Just $"Set props on per header cell"
      <> "\nType: Function(column)"
      <> "\nDefault: -"
    , propTyp:
      { required: false
      , typ: TypFn { effectful: true
                 , input: [ requiredPropTyp TypUnknown
                          ]
                 , output: requiredPropTyp TypUnit
                 }
      }
    }
  ]
