module Main exposing (..)

import Browser
import Debug exposing (log)
import FeatherIcons
import Html exposing (Html, a, button, div, form, input, label, li, span, text, ul)
import Html.Attributes exposing (autofocus, class, href, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder, Error(..), decodeString, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import List.Extra exposing (remove)
import Ports
import Regex



-- MODEL


type alias Reference =
    { name : String
    , link : String
    }


type alias Model =
    { openReference : Reference
    , referenceList : List Reference
    }


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    let
        references =
            case flags of
                Just referencesJson ->
                    decodeStoredReferences referencesJson

                Nothing ->
                    []
    in
    ( { openReference =
            { name = ""
            , link = ""
            }
      , referenceList = references
      }
    , Cmd.none
    )



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- UPDATE


type Msg
    = SaveReferenceName String
    | SaveReferenceLink String
    | AddReference
    | RemoveReference Reference


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveReferenceName name ->
            ( { model | openReference = { name = name, link = model.openReference.link } }, Cmd.none )

        SaveReferenceLink link ->
            ( { model | openReference = { name = model.openReference.name, link = link } }, Cmd.none )

        AddReference ->
            let
                ref =
                    model.openReference
            in
            if validReference ref then
                let
                    newReferences =
                        ref :: model.referenceList
                in
                ( { model
                    | openReference =
                        { name = ""
                        , link = ""
                        }
                    , referenceList = newReferences
                  }
                , saveReferences newReferences
                )

            else
                log "nothing"
                    ( model, Cmd.none )

        RemoveReference ref ->
            let
                newReferences =
                    remove ref model.referenceList
            in
            ( { openReference =
                    { name = ""
                    , link = ""
                    }
              , referenceList = newReferences
              }
            , saveReferences newReferences
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ Html.form
            [ class "section-add"
            , onSubmit AddReference
            ]
            [ div [ class "section-add__inputs" ]
                [ label [ class "section-add__label-text" ]
                    [ text "Text"
                    , input
                        [ value model.openReference.name
                        , type_ "text"
                        , class "section-add__input"
                        , autofocus True
                        , onInput SaveReferenceName
                        ]
                        []
                    ]
                , label [ class "section-add__label-link" ]
                    [ text "Link"
                    , input
                        [ value model.openReference.link
                        , type_ "text"
                        , class "section-add__input"
                        , onInput SaveReferenceLink
                        ]
                        []
                    ]
                ]
            , button
                [ class "section-add__submit"
                , type_ "submit"
                , onClick AddReference
                ]
                [ FeatherIcons.plusSquare
                    |> FeatherIcons.withClass "section-add__icon-add"
                    |> FeatherIcons.toHtml []
                ]
            ]
        , renderReferenceList model.referenceList
        ]



-- FUNCTIONS


renderReferenceList : List Reference -> Html Msg
renderReferenceList lst =
    lst
        |> List.map (\l -> renderReference l)
        |> ul [ class "list" ]


renderReference : Reference -> Html Msg
renderReference ref =
    li [ class "item" ]
        [ span [ class "item__actions" ]
            [ renderRemove ref
            , renderIcon ref
            ]
        , renderReferenceText ref
        ]


renderRemove : Reference -> Html Msg
renderRemove ref =
    button
        [ type_ "button"
        , onClick (RemoveReference ref)
        , class "item__remove"
        ]
        [ FeatherIcons.xSquare
            |> FeatherIcons.withClass "item__icon-remove"
            |> FeatherIcons.toHtml []
        ]


renderIcon : Reference -> Html Msg
renderIcon ref =
    if String.isEmpty ref.link then
        FeatherIcons.edit2
            |> FeatherIcons.withClass "item__icon-note"
            |> FeatherIcons.toHtml []

    else
        FeatherIcons.link
            |> FeatherIcons.withClass "item__icon-link"
            |> FeatherIcons.toHtml []


renderReferenceText : { name : String, link : String } -> Html Msg
renderReferenceText { name, link } =
    if validLink link then
        a [ href link ] [ text name ]

    else
        text name


validReference : { a | name : String, link : String } -> Bool
validReference { name, link } =
    not (String.isEmpty name)
        && (String.isEmpty link || validLink link)


validLink : String -> Bool
validLink link =
    Regex.contains properLink link


properLink : Regex.Regex
properLink =
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
        [ ( "name", Encode.string ref.name )
        , ( "link", Encode.string ref.link )
        ]


referencesDecoder : Decoder (List Reference)
referencesDecoder =
    Decode.list referenceDecoder


referenceDecoder : Decoder Reference
referenceDecoder =
    Decode.succeed Reference
        |> required "name" string
        |> required "link" string


decodeStoredReferences : String -> List Reference
decodeStoredReferences referencesJson =
    case decodeString referencesDecoder referencesJson of
        Ok references ->
            references

        Err _ ->
            []
