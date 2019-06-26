module Main exposing (HttpRequest(..), Model, Msg(..), Route(..), User, emptyUser, failureMessages, fromUrl, init, main, routeParser, subscriptions, topNav, update, userDecoder, view, viewLink)

import Base64
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string, top)



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
-- type HttpResponse
--     = UsersResponse (List User)


type HttpRequest
    = Failure
    | Loading
    | Success


type Route
    = Home
    | Login
    | About
    | Users
    | NotFound


type alias Model =
    { httpRequest : HttpRequest
    , users : List User
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model Loading [] key url, getUsersRequest )



-- HELPERS


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map About (s "about")
        , map Users (s "users")
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Url.Parser.parse routeParser url)


getUsersRequest : Cmd Msg
getUsersRequest =
    Http.request
        { method = "GET"
        , headers = []
        , url = "https://api.github.com/users"
        , body = Http.emptyBody
        , expect = Http.expectJson GotUsers usersDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- type alias UserInfo =
--     { email : String
--     , membership : Membership
--     }
-- type alias LoginResponse =
--     { token : Maybe String
--     , email : Maybe String
--     , membership : Maybe Membership
--     , message : Maybe String
--     }


type alias User =
    { id : Int
    , url : String
    , login : String
    , avatarUrl : String
    }


emptyUser =
    { id = 0
    , url = ""
    , login = ""
    , avatarUrl = ""
    }


userDecoder : Decoder User
userDecoder =
    Json.Decode.map4 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "login" Json.Decode.string)
        (Json.Decode.field "avatar_url" Json.Decode.string)


usersDecoder : Decoder (List User)
usersDecoder =
    Json.Decode.list userDecoder



-- userEncoder : User -> Json.Encode.Value
-- userEncoder user =
--     Json.Encode.object
--         [ ( "id", Json.Encode.string user.id )
--         , ( "url", Json.Encode.string user.url )
--         , ( "login", Json.Encode.string user.login )
--         , ( "avatar_url", Json.Encode.string user.avatarUrl )
--         ]


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GetUsers
    | GotUsers (Result Http.Error (List User))


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
            case fromUrl url of
                NotFound ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | url = url }
                    , Cmd.none
                    )

        GetUsers ->
            ( model, getUsersRequest )

        GotUsers response ->
            case response of
                Ok users ->
                    ( { model | httpRequest = Success, users = users }, Cmd.none )

                Err _ ->
                    ( { model | httpRequest = Failure }, Cmd.none )



-- GotCache cache ->
--     let
--         _ =
--             Debug.log "cache" cache
--     in
--     ( model, Cmd.none )
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- fromCache GotCache
    Sub.none



-- VIEW


viewLink : String -> String -> List String -> Html msg
viewLink path name classes =
    a [ class <| String.join " " classes, href path ]
        [ text name ]


failureMessages : HttpRequest -> Html Msg
failureMessages httpRequest =
    case httpRequest of
        Failure ->
            div [ class "alert alert-danger" ] [ text "Unable to contact the server" ]

        _ ->
            div [] []


topNav : Model -> Html Msg
topNav model =
    nav [ class "navbar navbar-expand-sm navbar-light bg-light" ]
        [ a [ class "navbar-brand" ]
            [ text "Elm Parcel Starter" ]
        , div []
            [ ul [ class "navbar-nav" ]
                [ li [ class "nav-item" ]
                    [ viewLink "/" "Home" [ "nav-link" ]
                    ]
                , li [ class "nav-item" ]
                    [ viewLink "/about" "About" [ "nav-link" ]
                    ]
                , li [ class "nav-item" ]
                    [ viewLink "/users" "Users" [ "nav-link" ]
                    ]
                ]
            ]
        ]


userTable : Model -> Html Msg
userTable model =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [ scope "col" ] [ text "ID" ]
                , th [ scope "col" ] [ text "Url" ]
                , th [ scope "col" ] [ text "Login" ]
                , th [ scope "col" ] [ text "Avatar" ]
                ]
            ]
        , tbody []
            (List.map
                (\user ->
                    tr []
                        [ td [ class "align-middle" ] [ text <| String.fromInt user.id ]
                        , td [ class "align-middle" ] [ text user.url ]
                        , td [ class "align-middle" ] [ text user.login ]
                        , td [ class "align-middle" ] [ img [ src user.avatarUrl, height 42, width 42 ] [] ]
                        ]
                )
                model.users
            )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "IP Web"
    , body =
        [ topNav model
        , div [ class "container my-5" ]
            [ text "The current URL is: "
            , b [] [ text (Url.toString model.url) ]
            , hr [] []
            , case fromUrl model.url of
                Home ->
                    text "Welcome Home!"

                About ->
                    text "This is the 'About' page"

                Users ->
                    userTable model

                _ ->
                    text "Page Not Found"
            ]
        ]
    }
