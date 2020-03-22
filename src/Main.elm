module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, form, img, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Encode as Encode
import List.Extra exposing (remove)
import Ports
import Regex



-- MODEL


type alias Reference =
    String


type alias Model =
    { openReference : Reference
    , referenceList : List Reference
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { openReference = ""
      , referenceList = []
      }
    , Cmd.none
    )



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- UPDATE


type Msg
    = SaveReference Reference
    | AddReference
    | RemoveReference Reference


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveReference ref ->
            ( { model | openReference = ref }, Cmd.none )

        AddReference ->
            if not (String.isEmpty model.openReference) then
                let
                    newReferences = model.openReference :: model.referenceList
                in
                    ( { model
                        | openReference = ""
                        , referenceList = newReferences
                    }
                    , saveReferences newReferences
                    )

            else
                ( model, Cmd.none )

        RemoveReference ref ->
            ( { openReference = ""
              , referenceList = remove ref model.referenceList
              }
            , Cmd.none
            )



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
        , img [ src ("https://www.google.com/s2/favicons?domain=" ++ ref) ] []
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


saveReferences : List Reference -> Cmd msg
saveReferences references =
    Encode.list referenceEncode references
        |> Encode.encode 0
        |> Ports.storeReferences


referenceEncode : Reference -> Encode.Value
referenceEncode ref =
    Encode.object
        [ ( "reference", Encode.string ref ) ]
