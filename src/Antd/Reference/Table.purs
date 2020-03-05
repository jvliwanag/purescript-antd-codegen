module Antd.Reference.Table
       ( TableProps
       , table
       , ColumnProps
       , column
       , Expandable
       , RowSelection
       , Scroll
       ) where

import Prelude

import Data.Function.Uncurried (Fn4)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4)
import Foreign (Foreign)
import Literals (StringLit, BooleanLit)
import React.Basic (JSX, ReactComponent, element)
import Untagged.Coercible (class Coercible, coerce)
import Untagged.Union (type (|+|), UndefinedOr)


type TableProps =
  {

  -- [table-layout](https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout) attribute of table element
  -- Types: - \| `auto` \| `fixed`
  -- Default: -<hr />`fixed` when header/columns are fixed, or using `column.ellipsis`
    tableLayout :: UndefinedOr (StringLit "auto" |+| StringLit "fixed")

  -- Whether to show all table borders
  -- Types: boolean
  -- Default: `false`
  , bordered :: UndefinedOr Boolean

  -- Columns of table
  -- Types: [ColumnProps](#Column)\[]
  -- Default: -
  , columns :: UndefinedOr (Array ColumnProps)

  -- Override default table elements
  -- Types: [TableComponents](https://git.io/fANxz)
  -- Default: -
  , components :: UndefinedOr Foreign

  -- Data record array to be displayed
  -- Types: any\[]
  -- Default: -
  , dataSource :: UndefinedOr (Array Foreign)

  -- Config expandable content
  -- Types: [expandable](#expandable)
  -- Default: -
  , expandable :: UndefinedOr Expandable

  -- Table footer renderer
  -- Types: Function(currentPageData)
  -- Default: -
  , footer :: UndefinedOr (Foreign -> JSX)

  -- Loading status of table
  -- Types: boolean\|[object](https://ant.design/components/spin-cn/#API) ([more](https://github.com/ant-design/ant-design/issues/4544#issuecomment-271533135))
  -- Default: `false`
  , loading :: UndefinedOr Boolean

  -- i18n text including filter, sort, empty text, etc
  -- Types: object
  -- Default: filterConfirm: 'Ok' <br> filterReset: 'Reset' <br> emptyText: 'No Data' <br> [Default](https: //github.com/ant-design/ant-design/issues/575#issuecomment-159169511)
  , locale :: UndefinedOr { filterConfirm :: String, filterReset :: String, emptyText :: String }

  -- Config of pagination. You can ref table pagination [config](#pagination) or full [`pagination`](/components/pagination/) document, hide it by setting it to `false`
  -- Types: object
  -- Default: -
  , pagination :: UndefinedOr Foreign

  -- Row's className
  -- Types: Function(record, index):string
  -- Default: -
  , rowClassName :: UndefinedOr Foreign

  -- Row's unique key, could be a string or function that returns a string
  -- Types: string\|Function(record):string
  -- Default: `key`
  , rowKey :: UndefinedOr String

  -- Row selection [config](#rowSelection)
  -- Types: object
  -- Default: null
  , rowSelection :: UndefinedOr RowSelection

  -- Whether the table can be scrollable, [config](#scroll)
  -- Types: object
  -- Default: -
  , scroll :: UndefinedOr Scroll

  -- Whether to show table header
  -- Types: boolean
  -- Default: `true`
  , showHeader :: UndefinedOr Boolean

  -- Size of table
  -- Types: `default` \| `middle` \| `small`
  -- Default: `default`
  , size :: UndefinedOr (StringLit "default" |+| StringLit "middle" |+| StringLit "small")

  -- Summary content
  -- Types: (currentData) => ReactNode
  -- Default: -
  , summary :: UndefinedOr (Foreign -> JSX)

  -- Table title renderer
  -- Types: Function(currentPageData)
  -- Default: -
  , title :: UndefinedOr (Foreign -> JSX)

  -- Callback executed when pagination, filters or sorter is changed
  -- Types: Function(pagination, filters, sorter, extra: { currentDataSource: [] })
  -- Default: -
  , onChange :: UndefinedOr (EffectFn4 Foreign Foreign Foreign { extra :: { currentDataSource :: Foreign } } Unit )

  -- Set props on per header row
  -- Types: Function(column, index)
  -- Default: -
  , onHeaderRow :: UndefinedOr (EffectFn2 Foreign Int Unit)

  -- Set props on per row
  -- Types: Function(record, index)
  -- Default: -
  , onRow :: UndefinedOr (EffectFn2 Foreign Int Unit)

  -- the render container of dropdowns in table
  -- Types: (triggerNode) => HTMLElement
  -- Default: `() => TableHtmlElement`
  , getPopupContainer :: UndefinedOr (Foreign -> JSX)

  -- supported sort way, could be `'ascend'`, `'descend'`
  -- Types: Array
  -- Default: `['ascend', 'descend']`
  , sortDirections :: UndefinedOr (Array (StringLit "ascend" |+| StringLit "descend"))
}

foreign import _table :: ReactComponent TableProps

table :: forall r. Coercible r TableProps => r -> JSX
table props = element _table (coerce props)

type ColumnProps =
  {
  -- specify which way that column is aligned
  -- Types: `left` \| `right` \| `center`
  -- Default: `left`
    align :: UndefinedOr (StringLit "left" |+| StringLit "right" |+| StringLit "center")

  -- ellipsis cell content, not working with sorter and filters for now.<br />tableLayout would be `fixed` when `ellipsis` is true.
  -- Types: boolean
  -- Default: false
  , ellipsis :: UndefinedOr Boolean

  -- className of this column
  -- Types: string
  -- Default: -
  , className :: UndefinedOr String

  -- Span of this column's title
  -- Types: number
  -- Default: -
  , colSpan :: UndefinedOr Number

  -- Display field of the data record, support nest path by string array
  -- Types: string \| string\[]
  -- Default: -
  , dataIndex :: UndefinedOr (String |+| Array String)

  -- Default filtered values
  -- Types: string\[]
  -- Default: - |
  , defaultFilteredValue :: UndefinedOr (Array String)

  -- Default order of sorted values
  -- Types: `ascend` \| `descend`
  -- Default: -
  , defaultSortOrder :: UndefinedOr (StringLit "ascend" |+| StringLit "descend")

  -- Customized filter overlay
  -- Types: React.ReactNode \| (props: [FilterDropdownProps](https://git.io/fjP5h)) => React.ReactNode
  -- Default: -
  , filterDropdown :: UndefinedOr (JSX |+| Foreign -> JSX)

  -- Whether `filterDropdown` is visible
  -- Types: boolean
  -- Default: -
  , filterDropdownVisible :: UndefinedOr Boolean

  -- Whether the `dataSource` is filtered
  -- Types: boolean
  -- Default: `false`
  , filtered :: UndefinedOr Boolean

  -- Controlled filtered value, filter icon will highlight
  -- Types: string\[]
  -- Default: -
  , filteredValue :: UndefinedOr (Array String)

  -- Customized filter icon
  -- Types: ReactNode\|(filtered: boolean) => ReactNode
  -- Default: `false`
  , filterIcon :: UndefinedOr (JSX |+| Boolean -> JSX)

  -- Whether multiple filters can be selected
  -- Types: boolean
  -- Default: `true`
  , filterMultiple :: UndefinedOr Boolean

  -- Filter menu config
  -- Types: object\[]
  -- Default: -
  , filters :: UndefinedOr Foreign

  -- (IE not support) Set column to be fixed: `true`(same as left) `'left'` `'right'`
  -- Types: boolean\|string
  -- Default: `false`
  , fixed :: UndefinedOr (Boolean |+| String)

  -- Unique key of this column, you can ignore this prop if you've set a unique `dataIndex`
  -- Types: string
  -- Default: -
  , key :: UndefinedOr String

  -- Renderer of the table cell. The return value should be a ReactNode, or an object for [colSpan/rowSpan config](#components-table-demo-colspan-rowspan)
  -- Types: Function(text, record, index) {}
  -- Default: -
  , render :: UndefinedOr (Fn4 String Foreign Int JSX Unit) -- TODO

  -- Sort function for local sort, see [Array.sort](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort)'s compareFunction. If you need sort buttons only, set to `true`
  -- Types: Function\|boolean
  -- Default: -
  , sorter :: UndefinedOr (Boolean |+| Foreign -> Int)

  -- Order of sorted values: `'ascend'` `'descend'` `false`
  -- Types: boolean\|string
  -- Default: -
  , sortOrder :: UndefinedOr (BooleanLit "false" |+| StringLit "ascend" |+| StringLit "descend")

  -- supported sort way, override `sortDirections` in `Table`, could be `'ascend'`, `'descend'`
  -- Types: Array
  -- Default: `['ascend', 'descend']`
  , sortDirections :: UndefinedOr (Array (StringLit "ascend" |+| StringLit "descend"))

  -- Title of this column
  -- Types: ReactNode\|({ sortOrder, sortColumn, filters }) => ReactNode
  -- Default: -
  , title :: UndefinedOr (JSX |+| ({ sortOrder :: Int, sortColumn :: Foreign, filters :: Foreign } -> JSX))

  -- Width of this column ([width not working?](https://github.com/ant-design/ant-design/issues/13825#issuecomment-449889241))
  -- Types: string\|number
  -- Default: -
  , width :: UndefinedOr (String |+| Number)

  -- Set props on per cell
  -- Types: Function(record, rowIndex)
  -- Default: -
  , onCell :: UndefinedOr (EffectFn2 Foreign Int Unit)

  -- Callback executed when the confirm filter button is clicked
  -- Types: Function
  -- Default: -
  , onFilter :: UndefinedOr (Effect Unit)

  -- Callback executed when `filterDropdownVisible` is changed
  -- Types: function(visible) {}
  -- Default: -
  , onFilterDropdownVisibleChange :: UndefinedOr (EffectFn1 Boolean Unit)

  -- Set props on per header cell
  -- Types: Function(column)
  -- Default: -
  , onHeaderCell :: UndefinedOr (EffectFn1 Foreign Unit)
  }

foreign import _column :: ReactComponent ColumnProps

column :: forall r. Coercible r ColumnProps => r -> JSX
column props = element _column (coerce props)

type Expandable =
  {
  -- The column contains children to display
  -- Types: string\[]
  -- Default: children
    childrenColumnName :: UndefinedOr (Array String)

  -- Expand all rows initially
  -- Types: boolean
  -- Default: `false`
  , defaultExpandAllRows :: UndefinedOr Boolean

  -- Initial expanded row keys
  -- Types: string\[]
  -- Default: -
  , defaultExpandedRowKeys :: UndefinedOr (Array String)

  -- Customize row expand Icon. Ref [example](https://codesandbox.io/s/fervent-bird-nuzpr)
  -- Types: Function(props):ReactNode
  -- Default: -
  , expandIcon :: UndefinedOr JSX

  -- Current expanded row keys
  -- Types: string\[]
  -- Default: -
  , expandedRowKeys :: UndefinedOr (Array String)

  -- Expanded container render for each row
  -- Types: Function(record, index, indent, expanded):ReactNode
  -- Default: -
  , expandedRowRender :: UndefinedOr (Fn4 Foreign Int Int Boolean JSX)

  -- Whether to expand row by clicking anywhere in the whole row
  -- Types: boolean
  -- Default: `false`
  , expandRowByClick :: UndefinedOr Boolean

  -- Indent size in pixels of tree data
  -- Types: number
  -- Default: 15
  , indentSize :: UndefinedOr Number

  -- Enable row can be expandable
  -- Types: (record) => boolean
  -- Default: -
  , rowExpandable :: UndefinedOr (Foreign -> Boolean)

  -- Callback executed when the row expand icon is clicked
  -- Types: Function(expanded, record)
  -- Default: -
  , onExpand :: UndefinedOr (EffectFn2 Boolean Foreign Unit)

  -- Callback executed when the expanded rows change
  -- Types: Function(expandedRows)
  -- Default: -
  , onExpandedRowsChange :: UndefinedOr (EffectFn1 Foreign Unit)
  }

--
-- rowSelection
--
type RowSelection =
  {
  -- Set the width of the selection column
  -- Types: string\|number
  -- Default: `60px`
    columnWidth :: UndefinedOr (String |+| Number)

  -- Set the title of the selection column
  -- Types: string\|React.ReactNode
  -- Default: -
  , columnTitle :: UndefinedOr (String |+| JSX)

  -- Fixed selection column on the left
  -- Types: boolean
  -- Default: -
  , fixed :: UndefinedOr Boolean

  -- Get Checkbox or Radio props
  -- Types: Function(record)
  -- Default: -
  , getCheckboxProps :: UndefinedOr (Foreign -> Foreign)

  -- Remove the default `Select All` and `Select Invert` selections when [custom selection](#components-table-demo-row-selection-custom)
  -- Types: boolean
  , -- Default: `false`
  hideDefaultSelections :: UndefinedOr Boolean

  -- Controlled selected row keys
  -- Types: string\[]\|number[]
  -- Default: \[]
  , selectedRowKeys :: UndefinedOr (Array String |+| Array Number)

  -- Custom selection [config](#rowSelection), only displays default selections when set to `true`
  -- Types: object\[]\|boolean
  -- Default: -
  , selections :: UndefinedOr (Array Foreign |+| Boolean)

  -- `checkbox` or `radio`
  -- Types: `checkbox` \| `radio`
  -- Default: `checkbox`
  , type :: UndefinedOr (StringLit "checkbox" |+| StringLit "radio")

  -- Callback executed when selected rows change
  -- Types: Function(selectedRowKeys, selectedRows)
  -- Default: -
  , onChange :: UndefinedOr (EffectFn2 (Array Foreign) (Array Foreign) Unit)

  -- Callback executed when select/deselect one row
  -- Types: Function(record, selected, selectedRows, nativeEvent)
  -- Default: -
  , onSelect :: UndefinedOr (EffectFn4 Foreign (Array Foreign) (Array Foreign) Foreign Unit)

  -- Callback executed when select/deselect all rows
  -- Types: Function(selected, selectedRows, changeRows)
  -- Default: -
  , onSelectAll :: UndefinedOr (EffectFn3 Foreign (Array Foreign) (Array Foreign) Unit)

  -- Callback executed when row selection is inverted
  -- Types: Function(selectedRows)
  -- Default: -
  , onSelectInvert :: UndefinedOr (EffectFn1 (Array Foreign) Unit)
  }

--
-- scroll
--

type Scroll =
  {
  -- Set horizontal scrolling, can also be used to specify the width of the scroll area, could be number, percent value, true and ['max-content'](https://developer.mozilla.org/zh-CN/docs/Web/CSS/width#max-content)
  -- Types: number \| true
  -- Default: -
    x :: UndefinedOr (Number |+| BooleanLit "true")

  -- Set vertical scrolling, can also be used to specify the height of the scroll area, could be number
  -- Types: number
  -- Default: -
  , y :: UndefinedOr Number

  -- Whether to scroll to the top of the table when paging, sorting, filtering changes
  -- Types: boolean
  -- Default: -
  , scrollToFirstRowOnChange :: UndefinedOr Boolean
  }

--
-- selection
--

type Selection =
  {
  -- Unique key of this selection
  -- Types: string
  -- Default: -
    key :: UndefinedOr String

  -- Display text of this selection
  -- Types: string\|React.ReactNode
  -- Default: -
  , text :: UndefinedOr (String |+| JSX)

  -- Callback executed when this selection is clicked
  -- Types: Function(changeableRowKeys)
  -- Default: -
  , onSelect :: UndefinedOr (EffectFn1 (Array String) Unit)
}
