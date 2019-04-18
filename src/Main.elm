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


type HttpRequest
    = Failure
    | Loading
    | Success LoginResponse


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



-- type alias Membership =
--     { super : Bool
--     , basic : Bool
--     , review : Bool
--     , dataEntry : Bool
--     , staff : Bool
--     }
-- type alias User =
--     { email : String
--     , membership : Membership
--     }


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
    { authenticate : HttpRequest
    , key : Nav.Key
    , url : Url.Url
    , email : String
    , password : String
    , loginSuccess : Bool
    , loginFailedMessage : String
    , batches : List Batch

    -- , token : String
    -- , user : User
    }



-- INIT
-- init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
-- init flags url key =
--     ( Model Loading key url
--     , Http.get
--         { url = "http://localhost:1337/ipweb/get_batches"
--         , expect = Http.expectString GotText
--         }
--     )


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



-- authenticate : Model -> Cmd Msg
-- authenticate model =
--     Http.request
--         { method = "POST"
--         , headers = [ Http.header "AUTHORIZATION" (Base64.encode (model.email ++ ":" ++ model.password)) ]
--         , url = "http://localhost:1337/ipweb/login"
--         , body = Http.emptyBody
--         -- , expect = Http.expectJson GotJson
--         , expect = Http.expectString GotText
--         , timeout = Nothing
--         , tracker = Nothing
--         }
-- UPDATE


type alias Membership =
    { super : Bool
    , basic : Bool
    , review : Bool
    , dataEntry : Bool
    , staff : Bool
    }


type alias User =
    { email : String
    , membership : Membership
    }


type alias LoginResponse =
    { token : Maybe String
    , user : Maybe User
    , message : Maybe String
    }


emptyUser =
    { email = ""
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
    Json.Decode.succeed Membership
        |> required "super" Json.Decode.bool
        |> required "basic" Json.Decode.bool
        |> required "review" Json.Decode.bool
        |> required "data_entry" Json.Decode.bool
        |> required "staff" Json.Decode.bool


userDecoder : Decoder User
userDecoder =
    Json.Decode.succeed User
        |> required "email" Json.Decode.string
        |> required "membership" membershipDecoder


loginDecoder : Decoder LoginResponse
loginDecoder =
    Json.Decode.map3 LoginResponse
        (Json.Decode.maybe (Json.Decode.field "token" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "user" userDecoder))
        (Json.Decode.maybe (Json.Decode.field "message" Json.Decode.string))


membershipEncoder : Membership -> Json.Encode.Value
membershipEncoder membership =
    Json.Encode.object
        [ ( "super", Json.Encode.bool <| membership.super )
        , ( "basic", Json.Encode.bool <| membership.basic )
        , ( "review", Json.Encode.bool <| membership.review )
        , ( "dataEntry", Json.Encode.bool <| membership.dataEntry )
        , ( "staff", Json.Encode.bool <| membership.staff )
        ]


userEncoder : User -> Json.Encode.Value
userEncoder user =
    Json.Encode.object
        [ ( "email", Json.Encode.string <| user.email )
        , ( "membership", membershipEncoder <| user.membership )
        ]


tokenUserEncoder : String -> User -> Json.Encode.Value
tokenUserEncoder token user =
    Json.Encode.object
        [ ( "token", Json.Encode.string <| token )
        , ( "user", userEncoder <| user )
        ]



-- loginResponseEncoder : LoginResponse -> Json.Encode.Value
-- loginResponseEncoder loginResponse =
--     Json.Encode.object
--         [ ( "token", Json.Encode.string <| loginResponse.token )
--         , ( "user", userEncoder <| loginResponse.user )
--         ]
-- Json.Decode.succeed LoginResponse
--     |> optional "message" Json.Decode.string ""
--     |> optional "token" Json.Decode.string ""
--     |> optional "user" userDecoder emptyUser
-- (Json.Decode.maybe (Json.Decode.field "token" Json.Decode.string))
-- (Json.Decode.maybe (Json.Decode.field "user" Json.Decode.string))
-- (Json.Decode.maybe (Json.Decode.field "message" Json.Decode.string))
-- { Json.Decode.field "token" Json.Decode.maybe
-- , Json.Decode.field "message" Json.Decode.maybe
-- }
-- [ Json.Decode.dict "token" Json.Decode.string
-- , Json.Decode.field "message" Json.Decode.string
-- ]
-- Json.Decode.succeed String
--     |> optional "token" Json.Decode.string "(no login token)"
--     |> optional "message" Json.Decode.string "(no message)"


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotResult (Result Http.Error LoginResponse)
    | SetEmail String
    | SetPassword String
    | GetAuthToken


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
            -- let
            --     -- _ =
            --     --     Debug.log "url" url
            --     _ =
            --         Debug.log "params" fromUrl url
            -- in
            -- let
            -- _ =
            --     Debug.log "route" (toString (fromUrl url))
            -- in
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

        GetAuthToken ->
            ( model
            , Http.request
                { method = "POST"
                , headers = [ Http.header "AUTHORIZATION" (Base64.encode (model.email ++ ":" ++ model.password)) ]
                , url = "http://localhost:1337/ipweb/login"
                , body = Http.emptyBody
                , expect = Http.expectJson GotResult loginDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        GotResult result ->
            let
                _ =
                    Debug.log "result" result
            in
            case result of
                Ok loginResponse ->
                    case loginResponse.message of
                        Just message ->
                            ( { model | authenticate = Success loginResponse, loginFailedMessage = message }, Cmd.none )

                        Nothing ->
                            case loginResponse.token of
                                Just token ->
                                    case loginResponse.user of
                                        -- we want to login, redirect, and fetch resources (3 commands)
                                        Just user ->
                                            ( { model | authenticate = Success loginResponse, loginFailedMessage = "", loginSuccess = True }
                                            , Cmd.batch
                                                [ Cache.cache <| tokenUserEncoder token user
                                                , Nav.pushUrl model.key "/"
                                                ]
                                            )

                                        Nothing ->
                                            ( model, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                Err _ ->
                    ( { model | authenticate = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


ajaxResponse : HttpRequest -> Html Msg
ajaxResponse httpRequest =
    case httpRequest of
        Failure ->
            div [ class "alert alert-danger" ] [ text "Unable to contact the server" ]

        Success response ->
            case response.message of
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
            , onClick GetAuthToken
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
            , ajaxResponse model.authenticate
            ]
        ]
    }
