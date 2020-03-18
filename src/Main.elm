module Main exposing (..)

import Browser
import List.Extra exposing (remove)
import Html exposing (Html, button, div, form, input, text, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)




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
      { openReference = ""
      , referenceList = model.openReference :: model.referenceList
      }
    RemoveReference ref ->
      { openReference = ""
      , referenceList = remove ref model.referenceList
      }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ renderReference model.referenceList
    , Html.form []
      [ input
        [ onInput SaveReference ] []
      , button
        [ type_ "button"
        , onClick AddReference ] [ text "+" ]
      ]
    ]



-- FUNCTIONS

renderReference : List Reference -> Html msg
renderReference lst =
    lst
       |> List.map (\l -> li [] [ text l ])
       |> ul []
