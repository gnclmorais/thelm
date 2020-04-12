module Main exposing (..)

import Debug exposing (log)

import Browser
import Html exposing (Html, a, button, div, form, img, input, label, li, text, ul)
import Html.Attributes exposing (autofocus, href, src, type_, value)
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
    ( { openReference =
            { name = ""
            , link = ""
            }
      , referenceList = []
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
            ( { openReference =
                    { name = ""
                    , link = ""
                    }
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
            [ label []
                [ text "Name"
                , input
                    [ value model.openReference.name
                    , autofocus True
                    , onInput SaveReferenceName
                    ]
                    []
                ]
            , label []
                [ text "Link"
                , input
                    [ value model.openReference.link
                    , onInput SaveReferenceLink
                    ]
                    []
                ]
            , button
                [ type_ "submit"
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
        , img [ src ("https://www.google.com/s2/favicons?domain=" ++ ref.link) ] []
        , renderReferenceText ref
        ]


renderReferenceText : { name : String, link : String } -> Html Msg
renderReferenceText { name, link } =
    if validLink link then
        a [ href link ] [ text name ]

    else
        text name


renderRemove : Reference -> Html Msg
renderRemove ref =
    button
        [ type_ "button"
        , onClick (RemoveReference ref)
        ]
        [ text "Remove" ]


validReference : { a | name : String, link : String } -> Bool
validReference { name, link } =
    not (String.isEmpty name)
        && (String.isEmpty link || validLink link)


isValidReference : { a | name : String } -> Bool
isValidReference { name } =
    String.isEmpty name


hasLinkReference : { a | link : String } -> Bool
hasLinkReference { link } =
    String.isEmpty link && validLink link


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
