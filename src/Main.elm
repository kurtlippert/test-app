module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Ajax
    = Failure
    | Loading
    | Success String


type Route
    = Batch Int
    | Document Int
    | Result Int


type alias Model =
    { ajax : Ajax
    , key : Nav.Key
    , url : Url.Url
    }



-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model Loading key url
    , Http.get
        { url = "http://localhost:1337/ipweb/get_batches"
        , expect = Http.expectString GotText
        }
    )



-- HELPERS


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Batch (s "batch" </> int)
        , map Document (s "document" </> int)
        , map Result (s "result" </> int)
        ]



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | ajax = Success fullText }, Cmd.none )

                Err _ ->
                    ( { model | ajax = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


view : Model -> Browser.Document Msg
view model =
    { title = "IP Web"
    , body =
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/"
            , viewLink "/batch"
            , viewLink "/batch/1"
            , viewLink "/document/2"
            ]
        ]
    }
