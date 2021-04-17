port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Json
import SHA1
import Zxcvbn exposing (Zxcvbn)


port checkPassword : String -> Cmd msg


port passwordChecked : (Json.Value -> msg) -> Sub msg


type PwnedCount
    = NotLoaded
    | Loading
    | Zero
    | One
    | Pwned Int
    | Error


type alias Model =
    { password : String
    , zxcvbn : Maybe Zxcvbn
    , pwnedCount : PwnedCount
    }


type Msg
    = SetPassword String
    | ZxcvbnChecked Json.Value
    | CheckPwned
    | PwnedResults (Result Http.Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Nothing NotLoaded, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPassword newPassword ->
            ( { model | password = newPassword, pwnedCount = NotLoaded }, checkPassword newPassword )

        ZxcvbnChecked json ->
            case Json.decodeValue Zxcvbn.decode json of
                Ok zxcvbn ->
                    ( { model | zxcvbn = Just zxcvbn }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CheckPwned ->
            ( { model | pwnedCount = Loading }, getPwnedMatches model.password )

        PwnedResults (Err _) ->
            ( model, Cmd.none )

        PwnedResults (Ok matches) ->
            ( { model | pwnedCount = pwnedCountFromResponse model.password matches }, Cmd.none )


sha1 : String -> String
sha1 s =
    SHA1.fromString s
        |> SHA1.toHex
        |> String.toUpper


pwnedCountFromResponse : String -> String -> PwnedCount
pwnedCountFromResponse password response =
    let
        suffix =
            sha1 password |> String.dropLeft 5

        match =
            String.lines response
                |> List.filter (String.startsWith suffix)
                |> List.head
                |> Maybe.map (String.dropLeft 36)
                |> Maybe.andThen String.toInt
    in
    case match of
        Nothing ->
            Zero

        Just 1 ->
            One

        Just count ->
            Pwned count


getPwnedMatches : String -> Cmd Msg
getPwnedMatches password =
    Http.get
        { url = "https://api.pwnedpasswords.com/range/" ++ String.left 5 (sha1 password)
        , expect = Http.expectString PwnedResults
        }


labelFor : String -> String -> Html msg
labelFor for name =
    Html.label [ class "block text-sm font-medium text-gray-700", Html.Attributes.for for ] [ text name ]


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit CheckPwned, style "width" "100%", style "display" "flex", style "flex-direction" "column" ]
            [ labelFor "password" "Password"
            , input
                [ id "password"
                , class "form-input"
                , placeholder "Choose a password"
                , autocomplete True
                , autofocus True
                , onInput SetPassword
                , maxlength 100
                , type_ "password"
                ]
                []
            , meter
                [ Html.Attributes.min "0"
                , Html.Attributes.max "4"
                , class "w-full rounded"
                , attribute "low" "2"
                , attribute "high" "3"
                , attribute "optimum" "4"
                , style "width" "100%"
                , model.zxcvbn
                    |> Maybe.map (String.fromInt << .score)
                    |> Maybe.withDefault "0"
                    |> value
                ]
                []
            , Html.button [ class "btn mt-2 px-3 py-2" ] [ text "Have I Been Pwned?" ]
            ]
        , case model.zxcvbn of
            Nothing ->
                text ""

            Just z ->
                viewZxcvbn z
        , case model.pwnedCount of
            NotLoaded ->
                text ""

            Error ->
                p [] [ text "Something went wrong while calling HIBP" ]

            Loading ->
                p [] [ text "Loading results from 'Have I been Pwned?'" ]

            Zero ->
                p [] [ text "Not found in the HIBP database!" ]

            One ->
                p [] [ text "One match in the HIBP database." ]

            Pwned c ->
                p [] [ text ("This password was found " ++ String.fromInt c ++ " times in the HIBP database.") ]
        ]


viewZxcvbn : Zxcvbn -> Html msg
viewZxcvbn zxcvbn =
    let
        warning =
            case zxcvbn.feedback.warning of
                Nothing ->
                    text ""

                Just w ->
                    p [ class "font-semibold" ] [ text w ]

        feedback =
            warning :: List.map (\s -> p [ class "text-sm" ] [ text s ]) zxcvbn.feedback.suggestions

        sequence =
            List.map
                (\( pattern, token ) ->
                    p []
                        [ span [ class "text-gray-500" ] [ text "Pattern: " ]
                        , text pattern
                        , text ", "
                        , span [ class "text-gray-500" ] [ text "Token: " ]
                        , text token
                        ]
                )
                zxcvbn.matchSequence
    in
    div [ id "zxcvbn" ]
        (feedback
            ++ sequence
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    passwordChecked ZxcvbnChecked


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
