module Types exposing (..)

type Msg = NoOp | Add | UpdateInput String | Toggle Int Bool | ClearCompleted | ToggleCompleted
type Visibility = All | Active
