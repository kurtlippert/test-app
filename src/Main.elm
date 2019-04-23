module Main exposing (Model, Msg(..), init, main, update, view)

import Base64
import Browser
import Browser.Navigation as Nav
import Cache exposing (..)
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


type HttpResponse
    = LoginResponse LoginResponsePayload


type HttpRequest
    = Failure
    | Loading
    | Success HttpResponse



-- type LoginRequest User
--     = Failure
--     | Loading
--     | Success User


type Route
    = Home
    | Login
    | AddBatch
    | AddDocument
    | AddResult
    | EditBatch Int
    | EditDocument Int
    | EditResult Int
    | NotFound


type alias Batch =
    { id : String
    , batchNumber : String
    , strainConstruct : String
    , productionDate : String
    , comment : String
    }


type alias Parameter =
    { name : String }


type alias Step =
    { name : String
    , parameters : List Parameter
    }


type alias Document =
    { id : String
    , uop : String
    , docNumber : String
    , version : String
    , steps : List Step
    }


type alias Model =
    { httpRequest : HttpRequest
    , key : Nav.Key
    , url : Url.Url
    , email : String
    , password : String
    , loginSuccess : Bool
    , loginFailedMessage : String
    , batches : List Batch
    }


emptyBatch =
    { id = ""
    , batchNumber = ""
    , strainConstruct = ""
    , productionDate = ""
    , comment = ""
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model Loading key url "" "" False "" [], Cmd.none )



-- HELPERS


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map AddBatch (s "batch")
        , map AddDocument (s "document")
        , map AddResult (s "result")
        , map EditBatch (s "batch" </> int)
        , map EditDocument (s "document" </> int)
        , map EditResult (s "result" </> int)
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
    | LoginRequest
    | GotLoginResponse (Result Http.Error LoginResponsePayload)
    | SetEmail String
    | SetPassword String
    | GotCache Json.Encode.Value



-- fetchResources : Cmd msg
-- fetchResources =
--     let
--         cache = Cache.getCache
--     in


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

                Login ->
                    let
                        _ =
                            Debug.log "route" "login"
                    in
                    ( { model | url = url }
                    , Cmd.none
                    )

                _ ->
                    ( { model | url = url }
                    , Cmd.none
                    )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        LoginRequest ->
            ( model
            , Http.request
                { method = "POST"
                , headers = [ Http.header "AUTHORIZATION" (Base64.encode (model.email ++ ":" ++ model.password)) ]
                , url = "http://localhost:1337/ipweb/login"
                , body = Http.emptyBody
                , expect = Http.expectJson GotLoginResponse loginResponsePayloadDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        GotLoginResponse loginResponsePayload ->
            let
                _ =
                    Debug.log "login_response_payload" loginResponsePayload
            in
            case loginResponsePayload of
                Ok payload ->
                    case payload.message of
                        Just message ->
                            ( { model | httpRequest = Success <| LoginResponse payload, loginFailedMessage = message }, Cmd.none )

                        -- because no message implies we successfully logged in
                        -- may want to change the api on that later (?)
                        Nothing ->
                            case payload.user of
                                Just user ->
                                    -- case responsePayload.user of
                                    --     -- we want to login, redirect, and fetch resources (3 commands)
                                    --     Just user ->
                                    --         ( { model | authenticate = Success loginResponse, loginFailedMessage = "", loginSuccess = True }
                                    --         , Cmd.batch
                                    --             [ Cache.cache <| tokenUserEncoder token user
                                    --             , Nav.pushUrl model.key "/"
                                    --             -- , Cache.getCache
                                    --             ]
                                    --         )
                                    --     Nothing ->
                                    --         ( model, Cmd.none )
                                    ( { model | httpRequest = Success <| LoginResponse payload, loginFailedMessage = "", loginSuccess = True }
                                    , Cmd.batch
                                        [ Cache.cache <| userEncoder user
                                        , Nav.pushUrl model.key "/"
                                        ]
                                    )

                                -- No message, but unable to get user (when would this happen?)
                                Nothing ->
                                    ( { model | loginFailedMessage = "Unable to properly get user information!" }, Cmd.none )

                Err _ ->
                    ( { model | httpRequest = Failure }, Cmd.none )

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


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


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


loginForm : Model -> Html Msg
loginForm model =
    div [ class "mb-5" ]
        [ h2 [] [ text "Login" ]
        , div [ class "form-group" ]
            [ label [] [ text "Email" ]
            , input
                [ type_ "text"
                , class "form-control"
                , Html.Attributes.required True
                , onInput SetEmail
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Password" ]
            , input
                [ type_ "password"
                , class "form-control"
                , Html.Attributes.required True
                , onInput SetPassword
                ]
                []
            ]
        , button
            [ type_ "button"
            , class "btn btn-primary"
            , Html.Attributes.required True
            , onClick LoginRequest
            ]
            [ text "Submit" ]
        ]


userInfo : Model -> Html Msg
userInfo model =
    div [] []


topNav : Model -> Html Msg
topNav model =
    nav [ class "navbar navbar-light bg-light" ]
        [ a [ class "navbar-brand" ]
            [ text "IP Web" ]
        , userInfo model
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "IP Web"
    , body =
        [ topNav model
        , div [ class "container my-5" ]
            [ loginForm model
            , text "The current URL is: "
            , b [] [ text (Url.toString model.url) ]
            , ul []
                [ viewLink "/"
                , viewLink "/login"
                , viewLink "/batch"
                , viewLink "/batch/1"
                , viewLink "/document/2"
                ]
            , failureMessages model.httpRequest
            ]
        ]
    }
