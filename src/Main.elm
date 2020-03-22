module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, a, button, div, form, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Extra exposing (remove)
import Regex



-- MODEL


type alias Reference =
    String


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
            if not (String.isEmpty model.openReference) then
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
                , autofocus True
                , onInput SaveReference
                ]
                []
            , button
                [ type_ "button"
                , onClick AddReference
                ]
                [ text "+" ]
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
        , renderReferenceText ref
        ]


renderReferenceText : String -> Html Msg
renderReferenceText ref =
    if validReference ref then
        a [ href ref ] [ text ref ]

    else
        text ref


renderRemove : Reference -> Html Msg
renderRemove ref =
    button
        [ type_ "button"
        , onClick (RemoveReference ref)
        ]
        [ text "Remove" ]


validReference : String -> Bool
validReference ref =
    Regex.contains link ref


link : Regex.Regex
link =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^(https?://)"
