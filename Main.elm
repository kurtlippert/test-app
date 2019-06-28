port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode
import Url
import Url.Parser exposing (Parser, int, map, oneOf, s, string, top)



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



-- PORTS


port printModel : Json.Encode.Value -> Cmd msg



-- MODEL


type HttpRequest
    = Failure String
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
    , selectedUser : Maybe User
    , gists : List Gist
    , key : Nav.Key
    , url : Url.Url
    }


emptyUser : User
emptyUser =
    { id = 0
    , url = ""
    , login = ""
    , avatarUrl = ""
    , gistsUrl = ""
    , reposUrl = ""
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model Loading [] Nothing [] key url, getUsersRequest )



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
        , url = "https://api.github.com/users?since=0&per_page=5"
        , body = Http.emptyBody
        , expect = Http.expectJson GotUsers usersDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| We obtain the 'gists\_url' in this form:
"<https://api.github.com/users/:username/gists{/gist_id}">
The idea is to replace the '{/gist\_id}' bit with the relevant id (or empty string)
if you want all the gists. Github makes it easy, as we just have to replace
that bit with what we want
-}
prepareGithubUserGistsUrl : String -> String -> String
prepareGithubUserGistsUrl gistIdOrEmptyString gistUrl =
    String.replace "{/gist_id}" gistIdOrEmptyString gistUrl


{-| We want to obtain all of a user's gists
So we'll make a call to 'prepareGithubUserGistsUrl' with empty string
-}
getUserGists : User -> Cmd Msg
getUserGists user =
    Http.request
        { method = "GET"
        , headers = []
        , url = prepareGithubUserGistsUrl "" user.gistsUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GotGists gistsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- TYPE ALIAS, DECODERS / ENCODERS


type alias User =
    { id : Int
    , url : String
    , login : String
    , avatarUrl : String
    , gistsUrl : String
    , reposUrl : String
    }


userDecoder : Decoder User
userDecoder =
    Json.Decode.map6 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "login" Json.Decode.string)
        (Json.Decode.field "avatar_url" Json.Decode.string)
        (Json.Decode.field "gists_url" Json.Decode.string)
        (Json.Decode.field "repos_url" Json.Decode.string)


usersDecoder : Decoder (List User)
usersDecoder =
    Json.Decode.list userDecoder


userEncoder : User -> Json.Encode.Value
userEncoder user =
    Json.Encode.object
        [ ( "id", Json.Encode.int user.id )
        , ( "url", Json.Encode.string user.url )
        , ( "login", Json.Encode.string user.login )
        , ( "avatar_url", Json.Encode.string user.avatarUrl )
        , ( "gists_url", Json.Encode.string user.gistsUrl )
        , ( "repos_url", Json.Encode.string user.reposUrl )
        ]


usersEncoder : List User -> Json.Encode.Value
usersEncoder users =
    Json.Encode.list userEncoder users


type alias Gist =
    { id : Int
    , htmlUrl : String
    }


gistDecoder : Decoder Gist
gistDecoder =
    Json.Decode.map2 Gist
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "html_url" Json.Decode.string)


gistsDecoder : Decoder (List Gist)
gistsDecoder =
    Json.Decode.list gistDecoder


gistEncoder : Gist -> Json.Encode.Value
gistEncoder gist =
    Json.Encode.object
        [ ( "id", Json.Encode.int gist.id )
        , ( "html_url", Json.Encode.string gist.htmlUrl )
        ]


gistsEncoder : List Gist -> Json.Encode.Value
gistsEncoder gists =
    Json.Encode.list gistEncoder gists


{-| Maybe a little misleading because we don't actually care about all of the model
Only the bits that have value printing out to the dev console
Note: currently only using this to print out the model ('PrintModel' message)
-}
modelEncoder : Model -> Json.Encode.Value
modelEncoder model =
    Json.Encode.object
        [ ( "users", usersEncoder model.users )
        , ( "selectedUser", userEncoder <| getSomeUser model.selectedUser )
        , ( "gists", gistsEncoder model.gists )
        ]


getSomeUser : Maybe User -> User
getSomeUser maybeUser =
    case maybeUser of
        Just user ->
            user

        Nothing ->
            emptyUser



-- UPDATE


type Msg
    = PrintModel
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GetUsers
    | GotUsers (Result Http.Error (List User))
    | GetGists
    | GotGists (Result Http.Error (List Gist))
    | SelectUser User


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrintModel ->
            ( model, printModel <| modelEncoder model )

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
            ( { model | httpRequest = Loading }, getUsersRequest )

        GotUsers response ->
            case response of
                Ok users ->
                    ( { model | httpRequest = Success, users = users }, Cmd.none )

                Err _ ->
                    ( { model | httpRequest = Failure "Failed to get users" }, Cmd.none )

        GetGists ->
            ( { model | httpRequest = Loading }, getUserGists <| getSomeUser model.selectedUser )

        GotGists response ->
            case response of
                Ok gists ->
                    ( { model | httpRequest = Success, gists = gists }, Cmd.none )

                Err _ ->
                    ( { model | httpRequest = Failure <| "Failed to get gists for user: " ++ (getSomeUser model.selectedUser).login }, Cmd.none )

        SelectUser user ->
            ( { model | selectedUser = Just user }, printModel <| modelEncoder model )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewLink : String -> String -> List String -> Html Msg
viewLink path name classes =
    a [ class <| String.join " " classes, href path ]
        [ text name ]


failureMessages : HttpRequest -> Html Msg
failureMessages httpRequest =
    case httpRequest of
        Failure message ->
            div [ class "alert alert-danger" ] [ text message ]

        _ ->
            div [] []


topNav : Html Msg
topNav =
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


userTable : Model -> List String -> Html Msg
userTable model classes =
    div [ class <| String.join " " classes ]
        [ div [ class "form-group" ]
            [ label [] [ text "Filter" ]
            , input [ class "form-control" ] []
            ]
        , table [ class "table table-hover" ]
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
                        tr [ attribute "data-toggle" "modal", attribute "data-target" "#userDetailsModal", onClick <| SelectUser user ]
                            [ td [ class "align-middle" ] [ text <| String.fromInt user.id ]
                            , td [ class "align-middle" ] [ text user.url ]
                            , td [ class "align-middle" ] [ text user.login ]
                            , td [ class "align-middle" ] [ img [ src user.avatarUrl, height 42, width 42 ] [] ]
                            ]
                    )
                    model.users
                )
            ]
        , div [ class "modal", attribute "tabindex" "-1", id "userDetailsModal", attribute "role" "dialog" ]
            [ div [ class "modal-dialog" ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-header" ]
                        [ h5 [ class "modal-title" ]
                            [ text "Modal title" ]
                        , button [ class "close", attribute "data-dismiss" "modal", type_ "button" ]
                            [ span [ attribute "aria-hidden" "true" ]
                                [ text "×" ]
                            ]
                        ]
                    , div [ class "modal-body" ]
                        [ p []
                            [ text "Modal body text goes here." ]
                        ]
                    , div [ class "modal-footer" ]
                        [ button [ class "btn btn-primary", type_ "button" ]
                            [ text "Save changes" ]
                        , button [ class "btn btn-secondary", attribute "data-dismiss" "modal", type_ "button" ]
                            [ text "Close" ]
                        ]
                    ]
                ]
            ]
        ]


{-| The reason I've added the 'userTable' to all views (just hidden)
is a hacky solution to getting the spacing to be consistent across
all the views. Removing this causes some slight spacing changes to be observed.
-}
mainContent : Model -> Route -> Html Msg
mainContent model route =
    case route of
        Home ->
            div [] [ text "Welcome Home!", userTable model [ "invisible" ] ]

        About ->
            div [] [ text "This is the 'About' page", userTable model [ "invisible" ] ]

        Users ->
            userTable model []

        _ ->
            div [] [ text "Page Not Found!", userTable model [ "invisible" ] ]


view : Model -> Browser.Document Msg
view model =
    { title = "IP Web"
    , body =
        [ topNav
        , div [ class "container my-5" ]
            [ text "The current URL is: "
            , b [] [ text (Url.toString model.url) ]
            , hr [] []
            , mainContent model (fromUrl model.url)
            ]
        ]
    }
