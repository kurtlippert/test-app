port module Main exposing (HttpRequest(..), HttpResponse(..), LoginResponsePayload, Membership, Model, Msg(..), Route(..), User, emptyUser, failureMessages, fromUrl, init, loginResponsePayloadDecoder, main, membershipDecoder, membershipEncoder, routeParser, setStorage, subscriptions, topNav, update, updateWithStorage, userDecoder, userEncoder, view, viewLink)

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


main : Program (Maybe Model) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


port cache : Json.Encode.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ cache newModel, cmds ]
    )



-- MODEL


type HttpResponse
    = LoginResponse LoginResponsePayload


type HttpRequest
    = Failure
    | Loading
    | Success HttpResponse


type Route
    = Home
    | Login
    | About
    | Users
    | NotFound


type alias Model =
    { httpRequest : HttpRequest
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model Loading key url, Cmd.none )



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


type alias Membership =
    { super : Bool
    , basic : Bool
    , review : Bool
    , dataEntry : Bool
    , staff : Bool
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
    { token : String
    , email : String
    , membership : Membership
    }


type alias LoginResponsePayload =
    { user : Maybe User
    , message : Maybe String
    }


emptyUser =
    { token = ""
    , email = ""
    , membership =
        { super = False
        , basic = False
        , review = False
        , dataEntry = False
        , staff = False
        }
    }


membershipDecoder : Decoder Membership
membershipDecoder =
    Json.Decode.map5 Membership
        (Json.Decode.field "super" Json.Decode.bool)
        (Json.Decode.field "basic" Json.Decode.bool)
        (Json.Decode.field "review" Json.Decode.bool)
        (Json.Decode.field "data_entry" Json.Decode.bool)
        (Json.Decode.field "staff" Json.Decode.bool)



-- userInfoDecoder : Decoder UserInfo
-- userInfoDecoder =
--     Json.Decode.map2 UserInfo
--         (Json.Decode.field "email" Json.Decode.string)
--         (Json.Decode.field "membership" membershipDecoder)


userDecoder : Decoder User
userDecoder =
    Json.Decode.map3 User
        (Json.Decode.field "token" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "membership" membershipDecoder)


loginResponsePayloadDecoder : Decoder LoginResponsePayload
loginResponsePayloadDecoder =
    Json.Decode.map2 LoginResponsePayload
        (Json.Decode.maybe (Json.Decode.field "user" userDecoder))
        (Json.Decode.maybe (Json.Decode.field "message" Json.Decode.string))


membershipEncoder : Membership -> Json.Encode.Value
membershipEncoder membership =
    Json.Encode.object
        [ ( "super", Json.Encode.bool membership.super )
        , ( "basic", Json.Encode.bool membership.basic )
        , ( "review", Json.Encode.bool membership.review )
        , ( "dataEntry", Json.Encode.bool membership.dataEntry )
        , ( "staff", Json.Encode.bool membership.staff )
        ]



-- userInfoEncoder : UserInfo -> Json.Encode.Value
-- userInfoEncoder userInfo =
--     Json.Encode.object
--         [ ( "email", Json.Encode.string <| userInfo.email )
--         , ( "membership", membershipEncoder <| userInfo.membership )
--         ]


userEncoder : User -> Json.Encode.Value
userEncoder user =
    Json.Encode.object
        [ ( "token", Json.Encode.string user.token )
        , ( "email", Json.Encode.string user.email )
        , ( "membership", membershipEncoder user.membership )
        ]



-- tokenUserEncoder : String -> User -> Json.Encode.Value
-- tokenUserEncoder token user =
--     Json.Encode.object
--         [ ( "token", Json.Encode.string <| token )
--         , ( "user", u <| user )
--         ]


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotCache Json.Encode.Value



-- fetchResources : Cmd msg
-- fetchResources =
--     let
--         cache = Cache.getCache
--     in


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- let
    --     _ =
    --         Debug.log "msg" msg
    -- in
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    -- let
                    --     _ =
                    --         Debug.log "url" url
                    -- in
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case fromUrl url of
                NotFound ->
                    ( model, Cmd.none )

                Login ->
                    let
                        _ =
                            Debug.log "route" "login"
                    in
                    ( { model | url = url }
                    , Cmd.none
                    )

                _ ->
                    let
                        _ =
                            Debug.log "url" url
                    in
                    ( { model | url = url }
                    , Cmd.none
                    )

        GotCache cache ->
            let
                _ =
                    Debug.log "cache" cache
            in
            ( model, Cmd.none )



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

        Success response ->
            case response of
                LoginResponse payload ->
                    case payload.message of
                        Just message ->
                            div [ class "alert alert-danger" ] [ text message ]

                        _ ->
                            div [] []

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


view : Model -> Browser.Document Msg
view model =
    { title = "IP Web"
    , body =
        [ topNav model
        , div [ class "container my-5" ]
            [ text "The current URL is: "
            , b [] [ text (Url.toString model.url) ]
            , hr [] []
            ]
        ]
    }
