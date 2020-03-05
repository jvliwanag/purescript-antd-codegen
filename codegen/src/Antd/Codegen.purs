module Antd.Codegen
       ( run
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

type Module
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
    , required :: Boolean
    , typ :: Typ
    }

data Typ
  = TypString
  | TypInt
  | TypNumber
  | TypBoolean
  | TypRef { name :: String }
  | TypUnknown
  | TypStringLit String
  | TypBooleanLit Boolean
  | TypOneOf (Array Typ)
  | TypArray Typ
  | TypFn { effectful :: Boolean
          , input :: Array Typ
          , output :: Typ
          }
  | TypRecord (Array ( { key :: String
                       , required :: Boolean
                       , typ :: Typ
                       }
                     ) )
  | TypNode
  | TypUnit

tableModule :: Module
tableModule =
  { name: "Table"
  , primaryProps:
    [ { name: "tableLayout"
      , description: "[table-layout](https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout) attribute of table element"
      , docType: Just "- | `auto` | `fixed`"
      , docDefault: Just "-<hr />`fixed` when header/columns are fixed, or using `column.ellipsis`"
      , required: false
      , typ: TypOneOf [ TypStringLit "auto", TypStringLit "fixed" ]
      }

    , { name: "bordered"
      , description: "Whether to show all table borders"
      , docType: Just "boolean"
      , docDefault: Just "`false`"
      , required: false
      , typ: TypBoolean
      }

    , { name: "columns"
      , description: "Columns of table"
      , docType: Just "[ColumnProps](#Column)[]"
      , docDefault: Just "-"
      , required: false
      , typ: TypArray (TypRef { name: "ColumnProps" })
      }

    , { name: "components"
      , description: "Override default table elements"
      , docType: Just "[TableComponents](https://git.io/fANxz)"
      , docDefault: Just "-"
      , required: false
      , typ: TypUnknown
      }

    , { name: "dataSource"
      , description: "Data record array to be displayed"
      , docType: Just "any[]"
      , docDefault: Just "-"
      , required: false
      , typ: TypArray TypUnknown
      }

    , { name: "expandable"
      , description: "Config expandable content"
      , docType: Just "[expandable](#expandable)"
      , docDefault: Just "-"
      , required: false
      , typ: TypRef ({ name: "Expandable" })
      }

    , { name: "footer"
      , description: "Table footer renderer"
      , docType: Just "Function(currentPageData)"
      , docDefault: Just "-"
      , required: false
      , typ: TypFn { effectful: false, input: [TypUnknown], output: TypNode }
      }

    , { name: "loading"
      , description: "Loading status of table"
      , docType: Just "boolean|[object](https://ant.design/components/spin-cn/#API) ([more](https://github.com/ant-design/ant-design/issues/4544#issuecomment-271533135))"
      , docDefault: Just "`false`"
      , required: false
      , typ: TypOneOf [ TypBoolean ]
      }

    , { name: "locale"
      , description: "i18n text including filter, sort, empty text, etc"
      , docType: Just "object"
      , docDefault: Just "filterConfirm: 'Ok' <br> filterReset: 'Reset' <br> emptyText: 'No Data' <br> [Default](https: //github.com/ant-design/ant-design/issues/575#issuecomment-159169511)"
      , required: false
      , typ: TypRecord
        [ { key: "filterConfirm"
          , required: false
          , typ: TypString
          }
        , { key: "filterReset"
          , required: false
          , typ: TypString
          }
        , { key: "emptyText"
          , required: false
          , typ: TypString
          }
        ]
      }

    , { name: "pagination"
      , description: "Config of pagination. You can ref table pagination [config](#pagination) or full [`pagination`](/components/pagination/) document, hide it by setting it to `false`"
      , docType: Just "object"
      , docDefault: Just "-"
      , required: false
      , typ: TypUnknown
      }

    , { name: "rowClassName"
      , description: "Row's className"
      , docType: Just "Function(record, index):string"
      , docDefault: Just "-"
      , required: false
      , typ: TypUnknown
      }

    , { name: "rowKey"
      , description: "Row's unique key, could be a string or function that returns a string"
      , docType: Just "string|Function(record):string"
      , docDefault: Just "`key`"
      , required: false
      , typ: TypString
      }

    , { name: "rowSelection"
      , description: "Row selection [config](#rowSelection)"
      , docType: Just "object"
      , docDefault: Just "null"
      , required: false
      , typ: TypRef { name: "RowSelection" }
      }

    , { name: "scroll"
      , description: "Whether the table can be scrollable, [config](#scroll)"
      , docType: Just "object"
      , docDefault: Just "-"
      , required: false
      , typ: TypRef { name: "Scroll" }
      }

    , { name: "showHeader"
      , description: "Whether to show table header"
      , docType: Just "boolean"
      , docDefault: Just "`true`"
      , required: false
      , typ: TypBoolean
      }

    , { name: "size"
      , description: "Size of table"
      , docType: Just "`default` | `middle` | `small`"
      , docDefault: Just "`default`"
      , required: false
      , typ: TypOneOf [ TypStringLit "default"
                      , TypStringLit "middle"
                      , TypStringLit "small"
                      ]
      }

    , { name: "summary"
      , description: "Summary content"
      , docType: Just "(currentData) => ReactNode"
      , docDefault: Just "-"
      , required: false
      , typ: TypFn { effectful: false
                   , input: [ TypUnknown ]
                   , output: TypNode
                   }
      }

    , { name: "title"
      , description: "Table title renderer"
      , docType: Just "Function(currentPageData)"
      , docDefault: Just "-"
      , required: false
      , typ: TypFn { effectful: false, input: [TypUnknown], output: TypNode }
      }

    , { name: "onChange"
      , description: "Callback executed when pagination, filters or sorter is changed"
      , docType: Just "Function(pagination, filters, sorter, extra: { currentDataSource: [] })"
      , docDefault: Just "-"
      , required: false
      , typ: TypFn { effectful: true
                   , input: [ TypUnknown
                            , TypUnknown
                            , TypUnknown
                            , TypUnknown
                            ]
                   , output: TypUnit
                   }
      }

    , { name: "onHeaderRow"
      , description: "Set props on per header row"
      , docType: Just "Function(column, index)"
      , docDefault: Just "-"
      , required: false
      , typ: TypFn { effectful: true
                   , input: [TypUnknown, TypInt, TypUnit]
                   , output: TypUnit
                   }
      }

    , { name: "onRow"
      , description: "Set props on per row"
      , docType: Just "Function(record, index)"
      , docDefault: Just "-"
      , required: false
      , typ: TypFn { effectful: true
                   , input: [ TypUnknown, TypInt ]
                   , output: TypUnit
                   }
      }

    , { name: "getPopupContainer"
      , description: "the render container of dropdowns in table"
      , docType: Just "(triggerNode) => HTMLElement"
      , docDefault: Just "`() => TableHtmlElement`"
      , required: false
      , typ: TypFn { effectful: false
                   , input: [ TypUnknown ]
                   , output: TypUnknown
                   }
      }

    , { name: "sortDirections"
      , description: "supported sort way, could be `'ascend'`, `'descend'`"
      , docType: Just "Array"
      , docDefault: Just "`['ascend', 'descend']`"
      , required: false
      , typ: TypArray (TypOneOf [ TypStringLit "ascend", TypStringLit "descend" ])
      }
    ]
  , subComponents:
    [
    ]
  }

run :: Effect Unit
run = do
  log "Hello World"

columnPropsDef :: Array Prop
columnPropsDef =
  [ { name: "align"
    , description: "specify which way that column is aligned"
    , docType: Just "`left` | `right` | `center`"
    , docDefault: Just "`left`"
    , required: false
    , typ: TypOneOf [ TypStringLit "left",  TypStringLit "right", TypStringLit "center" ]
    }

  , { name: "ellipsis"
    , description: "ellipsis cell content, not working with sorter and filters for now.<br />tableLayout would be `fixed` when `ellipsis` is true."
    , docType: Just "boolean"
    , docDefault: Just "false"
    , required: false
    , typ: TypBoolean
    }

  , { name: "className"
    , description: "className of this column"
    , docType: Just "string"
    , docDefault: Just "-"
    , required: false
    , typ: TypString
    }

  , { name: "colSpan"
    , description: "Span of this column's title"
    , docType: Just "number"
    , docDefault: Just "-"
    , required: false
    , typ: TypNumber
    }

  , { name: "dataIndex"
    , description: "Display field of the data record, support nest path by string array"
    , docType: Just "string | string[]"
    , docDefault: Just "-"
    , required: false
    , typ: TypOneOf [ TypString, TypArray TypString ]
    }

  , { name: "defaultFilteredValue"
    , description: "Default filtered values"
    , docType: Just "string | []"
    , docDefault: Just "- |"
    , required: false
    , typ: TypArray TypString
    }

  , { name: "defaultSortOrder"
    , description: "Default order of sorted values"
    , docType: Just "`ascend` | `descend`"
    , docDefault: Just "-"
    , required: false
    , typ: TypOneOf [ TypStringLit "ascend", TypStringLit "descend" ]
    }

  , { name: "filterDropdown"
    , description: "Customized filter overlay"
    , docType: Just "React.ReactNode | (props: [FilterDropdownProps](https://git.io/fjP5h)) => React.ReactNode"
    , docDefault: Just "-"
    , required: false
    , typ: TypOneOf [ TypNode
                    , TypFn { effectful: false
                            , input: [ TypUnknown ]
                            , output: TypNode
                            }
                    ]

    }

  , { name: "filterDropdownVisible"
    , description: "Whether `filterDropdown` is visible"
    , docType: Just "boolean"
    , docDefault: Just "-"
    , required: false
    , typ: TypBoolean
    }

  , { name: "filtered"
    , description: "Whether the `dataSource` is filtered"
    , docType: Just "boolean"
    , docDefault: Just "`false`"
    , required: false
    , typ: TypBoolean
    }

  , { name: "filteredValue"
    , description: "Controlled filtered value, filter icon will highlight"
    , docType: Just "string[]"
    , docDefault: Just "-"
    , required: false
    , typ: TypArray TypString
    }

  , { name: "filterIcon"
    , description: "Customized filter icon"
    , docType: Just "ReactNode|(filtered: boolean) => ReactNode"
    , docDefault: Just "`false`"
    , required: false
    , typ: TypOneOf [ TypNode
                    , TypFn { effectful: false
                            , input: [ TypBoolean ]
                            , output: TypNode
                            }
                    ]
    }

  , { name: "filterMultiple"
    , description: "Whether multiple filters can be selected"
    , docType: Just "boolean"
    , docDefault: Just "`true`"
    , required: false
    , typ: TypBoolean
    }

  , { name: "filters"
    , description: "Filter menu config"
    , docType: Just "object[]"
    , docDefault: Just "-"
    , required: false
    , typ: TypArray TypUnknown
    }

  , { name: "fixed"
    , description: "(\"IE not support) Set column to be fixed: `true`(same as left) `'left'` `'right'`"
    , docType: Just "boolean|string"
    , docDefault: Just "`false`"
    , required: false
    , typ: TypOneOf [ TypBoolean
                    , TypString
                    ]
    }

  , { name: "key"
    , description: "Unique key of this column, you can ignore this prop if you've set a unique `dataIndex`"
    , docType: Just "string"
    , docDefault: Just "-"
    , required: false
    , typ: TypString
    }

  , { name: "render"
    , description:"Renderer of the table cell. The return value should be a ReactNode, or an object for [colSpan/rowSpan config](#components-table-demo-colspan-rowspan)"
    , docType: Just "Function(text, record, index) {}"
    , docDefault: Just "-"
    , required: false
    , typ: TypFn { effectful: false
                 , input: [ TypString, TypUnknown, TypInt, TypNode ]
                 , output: TypUnit
                 }
    }

  , { name: "sorter"
    , description: "Sort function for local sort, see [Array.sort](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort)'s compareFunction. If you need sort buttons only, set to `true`"
    , docType: Just "Function|boolean"
    , docDefault: Just "-"
    , required: false
    , typ: TypOneOf [ TypBoolean
                    , TypFn { effectful: false
                            , input: []
                            , output: TypUnknown
                            }
                    ]

    }

  , { name: "sortOrder"
    , description: "Order of sorted values: `'ascend'` `'descend'` `false`"
    , docType: Just "boolean|string"
    , docDefault: Just "-"
    , required: false
    , typ: TypOneOf [ TypBooleanLit false
                    , TypStringLit "ascend"
                    , TypStringLit "descend"
                    ]
    }

  , { name: "sortDirections"
    , description: "supported sort way, override `sortDirections` in `Table`, could be `'ascend'`, `'descend'`"
    , docType: Just "Array"
    , docDefault: Just "`['ascend', 'descend']`"
    , required: false
    , typ: TypArray ( TypOneOf [ TypStringLit "ascend"
                               , TypStringLit "descend"
                               ]
                    )
    }

  , { name: "title"
    , description: "Title of this column"
    , docType: Just "ReactNode|({ sortOrder, sortColumn, filters }) => ReactNode"
    , docDefault: Just "-"
    , required: false
    , typ: TypOneOf [ TypNode
                    , TypFn { effectful: false
                            , input: [ TypRecord [ { key: "sortOrder"
                                                   , required: false
                                                   , typ: TypInt
                                                   }
                                                 , { key: "sortColumn"
                                                   , required: false
                                                   , typ: TypUnknown
                                                   }
                                                 , { key: "filters"
                                                   , required: false
                                                   , typ: TypUnknown
                                                   }
                                                 ]
                                     ]
                            , output: TypNode
                            }
                    ]
    }

  , { name: "width"
    , description: "Width of this column ([width not working?](https://github.com/ant-design/ant-design/issues/13825#issuecomment-449889241))"
    , docType: Just "string|number"
    , docDefault: Just "-"
    , required: false
    , typ: TypOneOf [ TypString, TypNumber ]
    }

  , { name: "onCell"
    , description: "Set props on per cell"
    , docType: Just "Function(record, rowIndex)"
    , docDefault: Just "-"
    , required: false
    , typ: TypFn { effectful: true
                 , input: [ TypUnknown, TypInt ]
                 , output: TypUnit
                 }
    }

  , { name: "onFilter"
    , description: "Callback executed when the confirm filter button is clicked"
    , docType: Just "Function"
    , docDefault: Just "-"
    , required: false
    , typ: TypFn { effectful: true
                 , input: []
                 , output: TypUnit
                 }
    }

  , { name: "onFilterDropdownVisibleChange"
    , description: "Callback executed when `filterDropdownVisible` is changed"
    , docType: Just "function(visible) {}"
    , docDefault: Just "-"
    , required: false
    , typ: TypFn { effectful: true
                 , input: [ TypBoolean ]
                 , output: TypUnit
                 }
    }

  , { name: "onHeaderCell"
    , description:"Set props on per header cell"
    , docType: Just "Function(column)"
    , docDefault: Just "-"
    , required: false
    , typ: TypFn { effectful: true
                 , input: [ TypUnknown ]
                 , output: TypUnit
                 }
    }
  ]
