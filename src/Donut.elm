module Donut exposing (..)

import Array exposing (Array)
import Html.Styled as Html exposing (Html)
import Svg.Styled exposing (circle, svg)
import Svg.Styled.Attributes as SvgAttr
import Types exposing (..)
import Util


type alias Model =
    { chartData : List ChartsData
    , circleEndValues : Array Float
    }


init : List User -> Model
init usersList =
    let
        chartData users =
            users
                |> List.sortBy
                    (\user ->
                        let
                            crd =
                                user.card |> Maybe.withDefault 0.5
                        in
                        crd
                    )
                |> Util.toChartData 1 (usersList |> List.length |> toFloat)

        percentages =
            usersList
                |> chartData
                |> Array.fromList
                |> Array.map .percentage

        circleEndValues =
            percentages
                |> Array.indexedMap
                    (\i _ ->
                        if i == 0 then
                            0

                        else
                            percentages
                                |> Array.slice 0 i
                                |> Array.foldr (+) 0
                    )
    in
    { chartData = chartData usersList, circleEndValues = circleEndValues }


viewDonutData : Model -> List (Html msg)
viewDonutData model =
    [ circle [ SvgAttr.class "donut-hole", SvgAttr.cx "20", SvgAttr.cy "20", SvgAttr.r "15.91549430918954" ] []
    , circle [ SvgAttr.class "donut-ring", SvgAttr.cx "20", SvgAttr.cy "20", SvgAttr.r "15.91549430918954", SvgAttr.fill "transparent", SvgAttr.strokeWidth "3.5" ] []
    ]
        ++ (model.chartData
                |> List.indexedMap
                    (\int entry ->
                        let
                            endValue =
                                model.circleEndValues |> Array.get int |> Maybe.withDefault 0
                        in
                        circle
                            [ SvgAttr.class <|
                                "donut-segment"
                            , SvgAttr.stroke (Util.getHexColor int)
                            , SvgAttr.cx "20"
                            , SvgAttr.cy "20"
                            , SvgAttr.r "15.91549430918954"
                            , SvgAttr.fill "transparent"
                            , SvgAttr.strokeWidth "3.5"
                            , SvgAttr.strokeDasharray <| (entry.percentage |> String.fromFloat) ++ " " ++ (endValue |> String.fromFloat)
                            , SvgAttr.strokeDashoffset "25"
                            ]
                            []
                    )
           )


view : Model -> Html msg
view model =
    svg [ SvgAttr.viewBox "0 0 40 40", SvgAttr.class "donut" ]
        (viewDonutData model)
