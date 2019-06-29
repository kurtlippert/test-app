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
    | UserRoute Int
    | NotFound


type alias Model =
    { httpRequest : HttpRequest
    , users : List User
    , selectedUser : Maybe User
    , burgerMenuActive : Bool
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
    ( Model Loading [] Nothing False [] key url, getUsersRequest 0 5 "" )



-- HELPERS


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map About (s "about")
        , map Users (s "users")
        , map UserRoute (s "users" </> int)
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Url.Parser.parse routeParser url)


getUsersRequest : Int -> Int -> String -> Cmd Msg
getUsersRequest skip take query =
    let
        url =
            if query == "" then
                "https://api.github.com/users?since="
                    ++ String.fromInt skip
                    ++ "&per_page="
                    ++ String.fromInt take

            else
                "https://api.github.com/search/users?q="
                    ++ query
                    ++ "&since="
                    ++ String.fromInt skip
                    ++ "&per_page="
                    ++ String.fromInt take
    in
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson GotUsers usersDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| We obtain the 'gists\_url' in this form:
"<https://api.github.com/users/:username/gists{/gist_id}>"
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


isUserSelected : Maybe User -> Bool
isUserSelected maybeUser =
    case maybeUser of
        Just user ->
            True

        Nothing ->
            False



-- UPDATE


type Msg
    = PrintModel
    | PrintMessage String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GetUsers
    | GotUsers (Result Http.Error (List User))
    | GetGists
    | GotGists (Result Http.Error (List Gist))
    | SelectUser User
    | UnSelectUser
    | ToggleBurgerMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrintModel ->
            ( model, printModel <| modelEncoder model )

        PrintMessage someInput ->
            let
                _ =
                    Debug.log "input" someInput
            in
            ( model, Cmd.none )

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
            ( { model | httpRequest = Loading }, getUsersRequest 0 5 "" )

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

        UnSelectUser ->
            ( { model | selectedUser = Nothing }, printModel <| modelEncoder model )

        ToggleBurgerMenu ->
            ( { model | burgerMenuActive = not model.burgerMenuActive }, Cmd.none )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


topNav : Model -> Html Msg
topNav model =
    nav [ class "navbar is-light" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item font-weight-bold", href "/" ]
                [ text "Elm Parcel Starter" ]
            , span
                [ classList
                    [ ( "navbar-burger burger", True )
                    , ( "is-active", model.burgerMenuActive )
                    ]
                , attribute "data-target" "mainNavbar"
                , onClick ToggleBurgerMenu
                ]
                [ span [] []
                , span [] []
                , span [] []
                ]
            ]
        , div
            [ classList
                [ ( "navbar-menu", True )
                , ( "is-active", model.burgerMenuActive )
                ]
            , id "mainNavbar"
            ]
            [ div [ class "navbar-start" ]
                [ a [ class "navbar-item", href "/" ]
                    [ text "Home" ]
                , a [ class "navbar-item", href "/about" ]
                    [ text "About" ]
                , a [ class "navbar-item", href "/users" ]
                    [ text "Users" ]
                ]
            ]
        ]


userTable : Model -> List String -> Html Msg
userTable model classes =
    div [ class <| String.join " " classes ]
        [ div [ class "form-group" ]
            [ input [ class "input", placeholder "Filter", type_ "text", onInput PrintMessage ]
                []
            ]
        , table [ class "table is-hoverable is-fullwidth mt-3" ]
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
                        tr [ onClick <| SelectUser user ]
                            [ td [ class "align-middle" ] [ text <| String.fromInt user.id ]
                            , td [ class "align-middle" ] [ text user.url ]
                            , td [ class "align-middle" ] [ text user.login ]
                            , td [ class "align-middle" ] [ img [ src user.avatarUrl, height 42, width 42 ] [] ]
                            ]
                    )
                    model.users
                )
            ]
        , div
            [ classList
                [ ( "modal", True )
                , ( "is-active", isUserSelected model.selectedUser )
                ]
            ]
            [ div [ class "modal-background", onClick UnSelectUser ] []
            , div [ class "modal-content" ]
                [ div [ class "card" ]
                    [ userContent <|
                        getSomeUser model.selectedUser
                    ]
                ]
            , button [ class "modal-close is-large", onClick UnSelectUser ]
                []
            ]
        ]


{-| Right now just the content for the modal popup
We could reuse this for the "users/:userId" route
-}
userContent : User -> Html Msg
userContent user =
    div [ class "card-content" ]
        [ div [ class "media" ]
            [ div [ class "media-left" ]
                [ figure [ class "image is-48x48" ]
                    [ img [ alt "Placeholder image", src user.avatarUrl ]
                        []
                    ]
                ]
            , div [ class "media-content" ]
                [ p [ class "title is-4" ]
                    [ text user.login ]
                , p [ class "subtitle is-6" ]
                    [ text "@johnsmith" ]
                ]
            ]
        , div [ class "content" ]
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus nec iaculis mauris."
            , a []
                [ text "@bulmaio" ]
            , text ".      "
            , a [ href "#" ]
                [ text "#css" ]
            , a [ href "#" ]
                [ text "#responsive" ]
            , br []
                []
            , time [ datetime "2016-1-1" ]
                [ text "11:09 PM - 1 Jan 2016" ]
            ]
        ]


{-| The reason I've added the 'userTable' to all views (just hidden)
is a hacky solution to getting the spacing to be consistent across
all the views. If you remove it you'll see the spacing change from route to route
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

        UserRoute userId ->
            div [] [ text <| "User ID: " ++ String.fromInt userId, br [] [], text "(in future, make http request to get user info" ]

        _ ->
            div [] [ text "Page Not Found!", userTable model [ "invisible" ] ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Parcel Starter"
    , body =
        [ topNav model
        , div [ class "container m-5" ]
            [ text "The current URL is: "
            , b [] [ text (Url.toString model.url) ]
            , hr [] []
            , mainContent model (fromUrl model.url)
            ]
        ]
    }
