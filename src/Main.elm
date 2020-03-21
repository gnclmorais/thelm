module Main exposing (..)

import Browser
import List.Extra exposing (remove)
import Html exposing (Html, Attribute, button, div, form, input, text, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)

import Json.Decode as Decode




-- MODEL

type alias Reference = String

type alias Model =
  { openReference : Reference
  , referenceList : List Reference
  }

init : Model
init =
  { openReference = ""
  , referenceList = []
  }



-- MAIN


main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }



-- UPDATE

type Msg
  = SaveReference Reference
  | AddReference
  | RemoveReference Reference

update : Msg -> Model -> Model
update msg model =
  case msg of
    SaveReference ref ->
      { model | openReference = ref }
    AddReference ->
      if not(String.isEmpty model.openReference) then
        { model
        | openReference = ""
        , referenceList = model.openReference :: model.referenceList
        }
      else
        model
    RemoveReference ref ->
      { openReference = ""
      , referenceList = remove ref model.referenceList
      }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ renderReferenceList model.referenceList
    , Html.form
      [ onSubmit AddReference ]
      [ input
        [ value model.openReference
        , onInput SaveReference ] []
      , button
        [ type_ "button"
        , onClick AddReference ] [ text "+" ]
      ]
    ]



-- FUNCTIONS

renderReferenceList : List Reference -> Html Msg
renderReferenceList lst =
    lst
       |> List.map (\l -> renderReference l)
       |> ul []

renderReference : Reference -> Html Msg
renderReference ref =
  li []
    [ renderRemove ref
    , text ref ]

renderRemove : Reference -> Html Msg
renderRemove ref =
  button
    [ type_ "button"
    , onClick (RemoveReference ref)]
    [ text "Remove" ]
