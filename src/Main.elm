module Main exposing (..)

import View exposing (view)
import Browser
import Browser.Dom as Dom
import Html exposing (Html, Attribute, button, div, text, span, ul, input, li, hr)
import Html.Attributes exposing (class, autofocus, value, classList, id, disabled)
import Html.Events exposing (onClick, on, keyCode, onInput)
import Debug exposing (log)
import Task
import Types exposing (..)

type alias State = 
  { 
    entries : List Entry,
    field : String,
    visibility : Visibility
  }

type alias Entry = 
  {
    desc : String,
    completed : Bool,
    editing : Bool,
    id : Int
  }

newEntry desc id = 
  {
    desc = desc,
    completed = False,
    editing = False,
    id = id
  }

defaultState = 
  {
    entries = 
      [
        {
          desc = "First entry name",
          completed = False,
          editing = False,
          id = 1 
        }
      ],
    field = "",
    visibility = All
  }

completeEntry id completed entry =
  if id == entry.id then
    { entry | completed = completed }
  else
    entry

addNonEmpty state = 
  let
    maxId =
      state.entries
        |> List.map .id
        |> List.maximum
        |> Maybe.withDefault 1
  in
    if String.isEmpty state.field then
      state.entries
    else
      state.entries ++ [ newEntry state.field (maxId + 1)]

update : Msg -> State -> (State, Cmd Msg)
update msg state = 
  let 
    result = case msg of
      NoOp -> 
        state
      ToggleCompleted ->
        case state.visibility of
          All -> { state | visibility = Active }
          Active -> { state | visibility = All }
      ClearCompleted ->
        { 
          state | 
            entries = List.filter (not << .completed) state.entries,
            visibility = All
        }
      Toggle id completed -> 
        { state | entries = List.map (completeEntry id completed) state.entries }
      UpdateInput str -> 
        { state | field = str }
      Add -> 
        {
          state | 
            field = "",
            entries = addNonEmpty state
        }
  in (result, Task.attempt (always NoOp) (Dom.focus "taskinput"))

init : Int -> (State, Cmd Msg)
init flags = 
  (defaultState, Cmd.none)

main = 
  Browser.element { 
    init = init, 
    update = update, 
    view = view,
    subscriptions = (always Sub.none)
  }
