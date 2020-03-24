module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes as HA exposing (step, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Nonempty exposing (..)
import Tuple


main =
    Browser.sandbox { init = Model (fromElement 10) 2.0, update = update, view = view }


type Msg
    = Increment
    | Decrement
    | ChangeWeight Int String
    | ChangeDaysToDouble String


type Value t
    = ValueError String
    | Value t


type alias WeightCalculation =
    { weight : Int
    , percentage : Float
    , infections : Float
    }


type alias Calculations =
    { m : Float
    , rate : Float
    , weightCalculations : Nonempty WeightCalculation
    }


type alias Model =
    { weights : Nonempty Int
    , daysToDouble : Float
    }


calculations daysToDouble weights =
    let
        rate =
            2.0 ^ (7 / daysToDouble)

        n =
            weights |> length |> toFloat

        m =
            (rate ^ n) / (weights |> indexedMap (\idx -> \w -> (w |> toFloat) * (rate ^ (idx |> toFloat))) |> foldl1 (+))

        sum =
            weights |> foldl1 (+) |> toFloat

        weightCalculations =
            weights |> map (\w -> WeightCalculation w (toFloat w / sum) (toFloat w * m))
    in
    Calculations m rate weightCalculations


view model =
    let
        calc =
            calculations model.daysToDouble model.weights
    in
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , button [ onClick Increment ] [ text "+" ]
        , div []
            [ input [ type_ "range", value (model.daysToDouble |> String.fromFloat), HA.min "0.5", HA.max "14", step "0.5", onInput ChangeDaysToDouble ] []
            , text (model.daysToDouble |> String.fromFloat)
            ]
        , div []
            (calc.weightCalculations
                |> reverse
                |> indexedMap
                    (\revidx ->
                        \{ weight, percentage, infections } ->
                            let
                                idx =
                                    (model.weights |> length) - revidx - 1
                            in
                            div []
                                [ input [ type_ "range", value (weight |> String.fromInt), HA.min "0", HA.max "100", onInput (ChangeWeight idx) ] []
                                , text (percentage * 100 |> String.fromFloat)
                                , text "% "
                                , text (infections |> String.fromFloat)
                                ]
                    )
                |> toList
            )
        ]


update msg model =
    case msg of
        Increment ->
            { model | weights = model.weights |> cons 0 }

        Decrement ->
            let
                poppedWeights =
                    model.weights |> pop

                newWeights =
                    if poppedWeights |> toList |> List.all (\w -> w == 0) then
                        Nonempty 1 (poppedWeights |> tail)

                    else
                        poppedWeights
            in
            { model | weights = newWeights }

        ChangeWeight idx newValue ->
            case newValue |> String.toInt of
                Nothing ->
                    model

                Just n ->
                    { model
                        | weights =
                            let
                                overRideToOne =
                                    (n == 0)
                                        && (model.weights
                                                |> indexedMap
                                                    (\i ->
                                                        \w ->
                                                            if idx == i then
                                                                0

                                                            else
                                                                w
                                                    )
                                                |> toList
                                                |> List.all (\w -> w == 0)
                                           )

                                m =
                                    if overRideToOne then
                                        1

                                    else
                                        n
                            in
                            model.weights
                                |> indexedMap
                                    (\oIdx ->
                                        \w ->
                                            if idx == oIdx then
                                                m

                                            else
                                                w
                                    )
                    }

        ChangeDaysToDouble newValue ->
            case newValue |> String.toFloat of
                Nothing ->
                    model

                Just days ->
                    { model | daysToDouble = days }
