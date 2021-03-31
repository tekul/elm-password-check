port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as Json
import Zxcvbn exposing (Zxcvbn)


port checkPassword : String -> Cmd msg


port passwordChecked : (Json.Value -> msg) -> Sub msg


type alias Model =
    { password : String
    , zxcvbn : Maybe Zxcvbn
    }


type Msg
    = SetPassword String
    | PasswordChecked Json.Value


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPassword newPassword ->
            ( { model | password = newPassword }, checkPassword newPassword )

        PasswordChecked json ->
            case Json.decodeValue Zxcvbn.decode json of
                Ok zxcvbn ->
                    ( { model | zxcvbn = Just zxcvbn }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


labelFor : String -> String -> Html msg
labelFor for name =
    Html.label [ style "font-size" "0.875rem", style "font-weight" "500", Html.Attributes.for for ] [ text name ]


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ style "width" "100%", style "display" "flex", style "flex-direction" "column" ]
            [ labelFor "password" "Password"
            , input
                [ id "password"
                , placeholder "Choose a password"
                , attribute "autocomplete" "off"
                , onInput SetPassword
                , maxlength 100
                , type_ "password"
                ]
                []
            ]
        , meter
            [ Html.Attributes.min "0"
            , Html.Attributes.max "4"
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
        , case model.zxcvbn of
            Nothing ->
                text ""

            Just z ->
                viewZxcvbn z
        ]


viewZxcvbn : Zxcvbn -> Html msg
viewZxcvbn zxcvbn =
    let
        warning =
            case zxcvbn.feedback.warning of
                Nothing ->
                    text ""

                Just w ->
                    p [] [ text w ]

        feedback =
            warning :: List.map (\s -> p [] [ text s ]) zxcvbn.feedback.suggestions

        sequence =
            List.map
                (\( pattern, token ) ->
                    p [] [ text ("Pattern: " ++ pattern ++ ", Token: " ++ token) ]
                )
                zxcvbn.matchSequence
    in
    div [ id "zxcvbn" ]
        (feedback
            ++ sequence
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    passwordChecked PasswordChecked


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
