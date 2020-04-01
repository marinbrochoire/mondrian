module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Random exposing (Generator)
import Random.Extra


type alias Row =
    { height : Float
    , cells : List Cell
    }


type alias Grid =
    { rows : List Row }


type alias Cell =
    { width : Float
    , color : Color
    }


type Color
    = White
    | Red
    | Blue
    | Yellow


type alias Model =
    { grid : Grid }


type Msg
    = NoOp
    | NewGrid Grid


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, generateGrid 7 9 )


initialModel : Model
initialModel =
    { grid =
        { rows =
            [ { height = 30.0
              , cells =
                    [ { width = 55.0, color = White }
                    , { width = 15.0, color = Yellow }
                    , { width = 10.0, color = White }
                    , { width = 20.0, color = White }
                    ]
              }
            , { height = 40.0
              , cells =
                    [ { width = 35.0, color = White }
                    , { width = 15.0, color = Red }
                    , { width = 50.0, color = White }
                    ]
              }
            , { height = 30.0
              , cells =
                    [ { width = 30.0, color = Red }
                    , { width = 55.0, color = White }
                    , { width = 15.0, color = Red }
                    ]
              }
            ]
        }
    }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewGrid model.grid
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    div
        [ class "grid" ]
        (List.map viewRow grid.rows)


viewRow : Row -> Html msg
viewRow row =
    div
        [ class "grid--row"
        , style
            "height"
            (cssPercentage row.height)
        ]
        (List.map viewCell row.cells)


cssPercentage : Float -> String
cssPercentage float =
    Debug.toString float ++ "%"


viewCell : Cell -> Html msg
viewCell cell =
    let
        modifier =
            String.toLower (Debug.toString cell.color)
    in
    div
        [ class "grid--cell"
        , class ("grid--cell__" ++ modifier)
        , style "width" (cssPercentage cell.width)
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGrid grid ->
            ( { model | grid = grid }, Cmd.none )


randomInt : Generator Int
randomInt =
    Random.int 3 100


type alias RandomRow =
    { height : Int
    , widths : List Int
    , colors : List Color
    }


generateGrid : Int -> Int -> Cmd Msg
generateGrid rows cols =
    let
        heights =
            Random.list rows randomInt

        widths =
            Random.list cols randomInt

        randomRows =
            Random.pair heights widths
                |> Random.andThen
                    (\( h, w ) ->
                        List.map
                            (\height ->
                                Random.map
                                    (RandomRow height w)
                                    (Random.list cols randomColor)
                            )
                            h
                            |> Random.Extra.combine
                    )

        gridGenerator =
            Random.map buildGrid randomRows
    in
    Random.generate NewGrid gridGenerator


randomColor : Generator Color
randomColor =
    Random.weighted
        ( 5.0, Red )
        [ ( 5.0, Blue )
        , ( 5.0, Yellow )
        , ( 85.0, White )
        ]


buildGrid : List RandomRow -> Grid
buildGrid randomRows =
    let
        totalHeight =
            List.map .height randomRows
                |> List.sum
    in
    { rows = List.map (buildRow totalHeight) randomRows }


buildRow : Int -> RandomRow -> Row
buildRow totalHeight { height, widths, colors } =
    let
        totalWidth =
            List.sum widths

        toPercentage value total =
            toFloat value * 100 / toFloat total

        result =
            List.map2 Tuple.pair widths colors
                |> List.map
                    (\( width, color ) ->
                        { width = toPercentage width totalWidth
                        , color = color
                        }
                    )
    in
    { height = toPercentage height totalHeight
    , cells = result
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
