module View exposing (..)

import Html exposing (Html, Attribute, button, div, text, span, ul, input, li, hr)
import Html.Attributes exposing (class, autofocus, value, classList, id, disabled)
import Json.Decode as Decode 
import Html.Events exposing (onClick, on, keyCode, onInput)
import String.Format
import Types exposing (..)

view state =
  div []
    [
      div [ class "row" ]
        [ 
          div [ class "col-12" ]
            [
              input 
                [ 
                  id "taskinput",
                  class "w-100 form-control", 
                  onEnter Add,
                  autofocus True,
                  onInput UpdateInput,
                  value state.field
                ] 
                [ ]
            ]
        ],
      div
        []
        (getEntriesForDisplay state),
      hr [] [],
      div [ class "row" ]
        [ 
          div 
            [ class "col-12" ] 
            [ 
              completedButton state.entries, 
              toggleButton state.visibility state.entries
            ]
        ]
    ]

viewEntries entry =
  div 
    [ 
      class "row my-2", 
      classList [ ("completed", entry.completed) ] ]
    [
      div 
        [ class "col-2" ]
        [
          button 
            [ 
              class "btn btn-info",
              onClick (Toggle entry.id (not entry.completed))
            ]
            [ text "✓" ]
        ],
      div 
        [ class "col-10" ] 
        [ 
          div 
            [ class "w-100 btn btn-primary text-left" ] 
            [ text entry.desc ] 
        ]
    ]

counterSuffix entries label =
  let
    completedCount =
      List.filter .completed entries |> List.length 
  in
    if completedCount > 0 then
      "{{ label }} ({{ counter }})"
        |> String.Format.namedValue "label" label
        |> String.Format.namedValue "counter" (String.fromInt completedCount)
    else
      label

getEntriesForDisplay state =
  let 
    entries = 
      case state.visibility of
         All -> state.entries
         Active -> state.entries |> List.filter (not << .completed)
  in
    List.map viewEntries entries


toggleButton visibility entries = 
  let
      label = 
        counterSuffix entries <|
          case visibility of
            All -> "Hide completed" 
            Active -> "Show completed" 
  in
    button 
      [ 
        class "btn-sm btn btn-success",
        disabled <| List.isEmpty <| List.filter .completed entries,
        onClick ToggleCompleted
      ]
      [ 
        text label
      ]

completedButton entries = 
  button 
    [ 
      class "btn-sm btn btn-success mr-1",
      disabled <| not <| List.any .completed entries,
      onClick ClearCompleted
    ]
    [ 
      counterSuffix entries "Clear completed" 
        |> text
    ]

onEnter : msg -> Attribute msg
onEnter msg =
  let 
    isEnter key = 
      if key == 13 then
        Decode.succeed msg
      else
        Decode.fail "Not enter"
  in on "keyup" (Decode.andThen isEnter keyCode)

