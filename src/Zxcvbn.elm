module Zxcvbn exposing (Feedback, Zxcvbn, decode)

import Json.Decode as Decode exposing (Decoder, field)
import Json.Decode.Pipeline exposing (required)


type alias Zxcvbn =
    { password : String
    , guesses : Float
    , guessesLog10 : Float
    , calcTime : Int
    , crackTimesSeconds : CrackTimesSeconds
    , crackTimesDisplay : CrackTimesDisplay
    , score : Int
    , feedback : Feedback
    , matchSequence : List ( String, String )
    }


type alias CrackTimesSeconds =
    { onlineThrottling100PerHour : Float
    , onlineNoThrottling10PerSecond : Float
    , offlineSlowHashing1e4PerSecond : Float
    , offlineFastHashing1e10PerSecond : Float
    }


type alias CrackTimesDisplay =
    { onlineThrottling100PerHour : String
    , onlineNoThrottling10PerSecond : String
    , offlineSlowHashing1e4PerSecond : String
    , offlineFastHashing1e10PerSecond : String
    }


type alias Feedback =
    { warning : Maybe String
    , suggestions : List String
    }


decode : Decoder Zxcvbn
decode =
    Decode.succeed Zxcvbn
        |> required "password" Decode.string
        |> required "guesses" Decode.float
        |> required "guesses_log10" Decode.float
        |> required "calc_time" Decode.int
        |> required "crack_times_seconds" decodeCrackTimesSeconds
        |> required "crack_times_display" decodeCrackTimesDisplay
        |> required "score" Decode.int
        |> required "feedback" decodeZxcvbnFeedback
        |> required "sequence" (Decode.list decodeMatch)


decodeCrackTimesSeconds : Decoder CrackTimesSeconds
decodeCrackTimesSeconds =
    Decode.map4 CrackTimesSeconds
        (field "online_throttling_100_per_hour" Decode.float)
        (field "online_no_throttling_10_per_second" Decode.float)
        (field "offline_slow_hashing_1e4_per_second" Decode.float)
        (field "offline_fast_hashing_1e10_per_second" Decode.float)


decodeCrackTimesDisplay : Decoder CrackTimesDisplay
decodeCrackTimesDisplay =
    Decode.map4 CrackTimesDisplay
        (field "online_throttling_100_per_hour" Decode.string)
        (field "online_no_throttling_10_per_second" Decode.string)
        (field "offline_slow_hashing_1e4_per_second" Decode.string)
        (field "offline_fast_hashing_1e10_per_second" Decode.string)


decodeZxcvbnFeedback : Decoder Feedback
decodeZxcvbnFeedback =
    Decode.map2 Feedback
        (field "warning" Decode.string
            |> Decode.map
                (\warning ->
                    if String.isEmpty warning then
                        Nothing

                    else
                        Just warning
                )
        )
        (field "suggestions" (Decode.list Decode.string))


decodeMatch : Decoder ( String, String )
decodeMatch =
    Decode.map2 Tuple.pair
        (field "pattern" Decode.string)
        (field "token" Decode.string)
