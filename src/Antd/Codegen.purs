module Antd.Codegen
       ( run
       ) where

import Prelude

import Antd.Codegen.ModuleBundler (createPSModule)
import Antd.Codegen.PSPrinter (printModule)
import Antd.Codegen.Types (AntModule, Prop, Typ(..), requiredPropTyp)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

tableModule :: AntModule
tableModule =
  { primaryComponent:
    { name: "Table"
    , props:
      [ { name: "tableLayout"
        , docDescription: Just "[table-layout](https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout) attribute of table element"
        , docType: Just "- | `auto` | `fixed`"
        , docDefault: Just "-<hr />`fixed` when header/columns are fixed, or using `column.ellipsis`"
        , propTyp:
        { required: false
        , typ: TypOneOf [ TypStringLit "auto", TypStringLit "fixed" ]
        }
        }

      , { name: "bordered"
        , docDescription: Just "Whether to show all table borders"
        , docType: Just "boolean"
        , docDefault: Just "`false`"
        , propTyp:
        { required: false
        , typ: TypBoolean
        }
        }

      , { name: "columns"
        , docDescription: Just "Columns of table"
        , docType: Just "[ColumnProps](#Column)[]"
        , docDefault: Just "-"
        , propTyp:
        { required: false
        , typ: TypArray (TypRef { name: "ColumnProps" })
        }
        }

      , { name: "components"
        , docDescription: Just "Override default table elements"
        , docType: Just "[TableComponents](https://git.io/fANxz)"
        , docDefault: Just "-"
        , propTyp:
        { required: false
        , typ: TypUnknown
        }
        }

      , { name: "dataSource"
        , docDescription: Just "Data record array to be displayed"
        , docType: Just "any[]"
        , docDefault: Just "-"
        , propTyp:
        { required: false
        , typ: TypArray TypUnknown
        }
        }

      , { name: "expandable"
        , docDescription: Just "Config expandable content"
        , docType: Just "[expandable](#expandable)"
        , docDefault: Just "-"
        , propTyp:
        { required: false
        , typ: TypRef ({ name: "Expandable" })
        }
        }

      , { name: "footer"
        , docDescription: Just "Table footer renderer"
        , docType: Just "Function(currentPageData)"
        , docDefault: Just "-"
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
        , docDescription: Just "Loading status of table"
        , docType: Just "boolean|[object](https://ant.design/components/spin-cn/#API) ([more](https://github.com/ant-design/ant-design/issues/4544#issuecomment-271533135))"
        , docDefault: Just "`false`"
        , propTyp:
        { required: false
        , typ: TypOneOf [ TypBoolean ]
        }
        }

      , { name: "locale"
        , docDescription: Just "i18n text including filter, sort, empty text, etc"
        , docType: Just "object"
        , docDefault: Just "filterConfirm: 'Ok' <br> filterReset: 'Reset' <br> emptyText: 'No Data' <br> [Default](https: //github.com/ant-design/ant-design/issues/575#issuecomment-159169511)"
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
        , docDescription: Just "Config of pagination. You can ref table pagination [config](#pagination) or full [`pagination`](/components/pagination/) document, hide it by setting it to `false`"
        , docType: Just "object"
        , docDefault: Just "-"
        , propTyp:
        { required: false
        , typ: TypUnknown
        }
        }

      , { name: "rowClassName"
        , docDescription: Just "Row's className"
        , docType: Just "Function(record, index):string"
        , docDefault: Just "-"
        , propTyp:
        { required: false
        , typ: TypUnknown
        }
        }

      , { name: "rowKey"
        , docDescription: Just "Row's unique key, could be a string or function that returns a string"
        , docType: Just "string|Function(record):string"
        , docDefault: Just "`key`"
        , propTyp:
        { required: false
        , typ: TypString
        }
        }

      , { name: "rowSelection"
        , docDescription: Just "Row selection [config](#rowSelection)"
        , docType: Just "object"
        , docDefault: Just "null"
        , propTyp:
        { required: false
        , typ: TypRef { name: "RowSelection" }
        }
        }

      , { name: "scroll"
        , docDescription: Just "Whether the table can be scrollable, [config](#scroll)"
        , docType: Just "object"
        , docDefault: Just "-"
        , propTyp:
        { required: false
        , typ: TypRef { name: "Scroll" }
        }
        }

      , { name: "showHeader"
        , docDescription: Just "Whether to show table header"
        , docType: Just "boolean"
        , docDefault: Just "`true`"
        , propTyp:
        { required: false
        , typ: TypBoolean
        }
        }

      , { name: "size"
        , docDescription: Just "Size of table"
        , docType: Just "`default` | `middle` | `small`"
        , docDefault: Just "`default`"
        , propTyp:
        { required: false
        , typ: TypOneOf [ TypStringLit "default"
                        , TypStringLit "middle"
                        , TypStringLit "small"
                        ]
        }
        }

      , { name: "summary"
        , docDescription: Just "Summary content"
        , docType: Just "(currentData) => ReactNode"
        , docDefault: Just "-"
        , propTyp:
        { required: false
        , typ: TypFn { effectful: false
                     , input: [ requiredPropTyp TypUnknown ]
                     , output: requiredPropTyp TypNode
                     }
        }
        }

      , { name: "title"
        , docDescription: Just "Table title renderer"
        , docType: Just "Function(currentPageData)"
        , docDefault: Just "-"
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
        , docDescription: Just "Callback executed when pagination, filters or sorter is changed"
        , docType: Just "Function(pagination, filters, sorter, extra: { currentDataSource: [] })"
        , docDefault: Just "-"
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
        , docDescription: Just "Set props on per header row"
        , docType: Just "Function(column, index)"
        , docDefault: Just "-"
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
        , docDescription: Just "Set props on per row"
        , docType: Just "Function(record, index)"
        , docDefault: Just "-"
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
        , docDescription: Just "the render container of dropdowns in table"
        , docType: Just "(triggerNode) => HTMLElement"
        , docDefault: Just "`() => TableHtmlElement`"
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
        , docDescription: Just "supported sort way, could be `'ascend'`, `'descend'`"
        , docType: Just "Array"
        , docDefault: Just "`['ascend', 'descend']`"
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

run :: Effect Unit
run = do
  log sampleOut

sampleOut :: String
sampleOut =
  tableModule # createPSModule # printModule

columnPropsDef :: Array Prop
columnPropsDef =
  [ { name: "align"
    , docDescription: Just "specify which way that column is aligned"
    , docType: Just "`left` | `right` | `center`"
    , docDefault: Just "`left`"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypStringLit "left",  TypStringLit "right", TypStringLit "center" ]
      }
    }

  , { name: "ellipsis"
    , docDescription: Just "ellipsis cell content, not working with sorter and filters for now.<br />tableLayout would be `fixed` when `ellipsis` is true."
    , docType: Just "boolean"
    , docDefault: Just "false"
    , propTyp:
      { required: false
      , typ: TypBoolean
      }
    }

  , { name: "className"
    , docDescription: Just "className of this column"
    , docType: Just "string"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypString
      }
    }

  , { name: "colSpan"
    , docDescription: Just "Span of this column's title"
    , docType: Just "number"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypNumber
      }
    }

  , { name: "dataIndex"
    , docDescription: Just "Display field of the data record, support nest path by string array"
    , docType: Just "string | string[]"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypString, TypArray TypString ]
      }
    }

  , { name: "defaultFilteredValue"
    , docDescription: Just "Default filtered values"
    , docType: Just "string | []"
    , docDefault: Just "- |"
    , propTyp:
      { required: false
      , typ: TypArray TypString
      }
    }

  , { name: "defaultSortOrder"
    , docDescription: Just "Default order of sorted values"
    , docType: Just "`ascend` | `descend`"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypStringLit "ascend", TypStringLit "descend" ]
      }
    }

  , { name: "filterDropdown"
    , docDescription: Just "Customized filter overlay"
    , docType: Just "React.ReactNode | (props: [FilterDropdownProps](https://git.io/fjP5h)) => React.ReactNode"
    , docDefault: Just "-"
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
    , docDescription: Just "Whether `filterDropdown` is visible"
    , docType: Just "boolean"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypBoolean
      }
    }

  , { name: "filtered"
    , docDescription: Just "Whether the `dataSource` is filtered"
    , docType: Just "boolean"
    , docDefault: Just "`false`"
    , propTyp:
      { required: false
      , typ: TypBoolean
      }
    }

  , { name: "filteredValue"
    , docDescription: Just "Controlled filtered value, filter icon will highlight"
    , docType: Just "string[]"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypArray TypString
      }
    }

  , { name: "filterIcon"
    , docDescription: Just "Customized filter icon"
    , docType: Just "ReactNode|(filtered: boolean) => ReactNode"
    , docDefault: Just "`false`"
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
    , docDescription: Just "Whether multiple filters can be selected"
    , docType: Just "boolean"
    , docDefault: Just "`true`"
    , propTyp:
      { required: false
      , typ: TypBoolean
      }
    }

  , { name: "filters"
    , docDescription: Just "Filter menu config"
    , docType: Just "object[]"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypArray TypUnknown
      }
    }

  , { name: "fixed"
    , docDescription: Just "(\"IE not support) Set column to be fixed: `true`(same as left) `'left'` `'right'`"
    , docType: Just "boolean|string"
    , docDefault: Just "`false`"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypBoolean
                    , TypString
                    ]
      }
    }

  , { name: "key"
    , docDescription: Just "Unique key of this column, you can ignore this prop if you've set a unique `dataIndex`"
    , docType: Just "string"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypString
      }
    }

  , { name: "render"
    , docDescription: Just"Renderer of the table cell. The return value should be a ReactNode, or an object for [colSpan/rowSpan config](#components-table-demo-colspan-rowspan)"
    , docType: Just "Function(text, record, index) {}"
    , docDefault: Just "-"
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
    , docDescription: Just "Sort function for local sort, see [Array.sort](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort)'s compareFunction. If you need sort buttons only, set to `true`"
    , docType: Just "Function|boolean"
    , docDefault: Just "-"
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
    , docDescription: Just "Order of sorted values: `'ascend'` `'descend'` `false`"
    , docType: Just "boolean|string"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypBooleanLit false
                    , TypStringLit "ascend"
                    , TypStringLit "descend"
                    ]
      }
    }

  , { name: "sortDirections"
    , docDescription: Just "supported sort way, override `sortDirections` in `Table`, could be `'ascend'`, `'descend'`"
    , docType: Just "Array"
    , docDefault: Just "`['ascend', 'descend']`"
    , propTyp:
      { required: false
      , typ: TypArray ( TypOneOf [ TypStringLit "ascend"
                               , TypStringLit "descend"
                               ]
                    )
      }
    }

  , { name: "title"
    , docDescription: Just "Title of this column"
    , docType: Just "ReactNode|({ sortOrder, sortColumn, filters }) => ReactNode"
    , docDefault: Just "-"
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
    , docDescription: Just "Width of this column ([width not working?](https://github.com/ant-design/ant-design/issues/13825#issuecomment-449889241))"
    , docType: Just "string|number"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypOneOf [ TypString, TypNumber ]
      }
    }

  , { name: "onCell"
    , docDescription: Just "Set props on per cell"
    , docType: Just "Function(record, rowIndex)"
    , docDefault: Just "-"
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
    , docDescription: Just "Callback executed when the confirm filter button is clicked"
    , docType: Just "Function"
    , docDefault: Just "-"
    , propTyp:
      { required: false
      , typ: TypFn { effectful: true
                   , input: []
                   , output: requiredPropTyp TypUnit
                   }
      }
    }

  , { name: "onFilterDropdownVisibleChange"
    , docDescription: Just "Callback executed when `filterDropdownVisible` is changed"
    , docType: Just "function(visible) {}"
    , docDefault: Just "-"
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
    , docDescription: Just"Set props on per header cell"
    , docType: Just "Function(column)"
    , docDefault: Just "-"
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
