module Main exposing (main)

import Browser exposing (Document, document)
import Dict
import GenGarden
import Html
import Maybe exposing (withDefault)
import Random exposing (Seed, initialSeed, int, maxInt, step)



-- -----------------------------------------------------------------
-- -----------------------------------------------------------------
-- Start Gen Garden


{-| Each Slider record will generate a slider to allow you to change a
-- draw setting at run time. You can have up to 10 sliders.
-}
mySliders : List GenGarden.Slider
mySliders =
    [ { label = "Trail 1"
      , max = 1
      , min = 0
      , step = 1
      , value = 1
      }
    , { label = "Trail 2"
      , max = 1
      , min = 0
      , step = 1
      , value = 0
      }
    , { label = "x1 Magnitude"
      , max = 50.0
      , min = 1.0
      , step = 0.2
      , value = 35.0
      }
    , { label = "x1 Frequency"
      , max = 40.0
      , min = 0
      , step = 0.25
      , value = 5
      }
    , { label = "y1 Magnitude"
      , max = 80.0
      , min = 1.0
      , step = 1
      , value = 45
      }
    , { label = "y1 Frequency"
      , max = 40.0
      , min = 0
      , step = 0.25
      , value = 5
      }
    ]


slow =
    0.45


{-| Redraw the image area, called every tick
-}
drawFrame : Dict.Dict String Float -> Float -> List (GenGarden.Drawing msg)
drawFrame settings ticks =
    let
        ts =
            round ticks

        trail1 =
            if (Dict.get "Trail 1" settings |> withDefault 1.0) > 0.0 then
                drawTrail "red" settings x1 y1 ticks

            else
                []

        trail2 =
            if (Dict.get "Trail 2" settings |> withDefault 1.0) > 0.0 then
                drawTrail "blue" settings x2 y2 (ticks * slow)

            else
                []
    in
    List.map
        (drawLine "purple" settings x1 y1 x2 y2)
        (floatRange ticks (ticks + 10))
        ++ trail1
        ++ trail2


{-| A Plotter returns a float, given a list of settings and a time.
Use Plotters to vary a coordinate or value over time.
-}
type alias Plotter =
    Dict.Dict String Float -> Float -> Float


drawLine :
    String
    -> Dict.Dict String Float
    -> Plotter
    -> Plotter
    -> Plotter
    -> Plotter
    -> Float
    -> GenGarden.Drawing msg
drawLine color settings xPlot1 yPlot1 xPlot2 yPlot2 ticks =
    GenGarden.line
        ( xPlot1 settings ticks, yPlot1 settings ticks )
        ( xPlot2 settings (ticks * slow), yPlot2 settings (ticks * slow) )
        color
        []
        []


drawTrail :
    String
    -> Dict.Dict String Float
    -> Plotter
    -> Plotter
    -> Float
    -> List (GenGarden.Drawing msg)
drawTrail color settings xPlot yPlot ticks =
    let
        dot : Float -> GenGarden.Drawing msg
        dot time =
            GenGarden.circle
                ( xPlot settings time, yPlot settings time )
                0.3
                color
                []
                []
    in
    List.map dot (floatRange ticks (ticks + 800))


{-| utility to give a list of floats, use like List.range
-}
floatRange : Float -> Float -> List Float
floatRange low high =
    List.range (round low) (round high)
        |> List.map (\x -> toFloat x)


x1 : Plotter
x1 settings ticks =
    let
        frq =
            Dict.get "x1 Frequency" settings |> withDefault 10

        mag =
            Dict.get "x1 Magnitude" settings |> withDefault 20
    in
    ((ticks / 4 |> sin) * 40) + ((ticks / frq |> sin) * mag)


y1 : Plotter
y1 settings ticks =
    let
        frq =
            Dict.get "y1 Frequency" settings |> withDefault 15

        mag =
            Dict.get "y1 Magnitude" settings |> withDefault 55
    in
    (ticks / 4 |> cos) * 40 + ((ticks / frq |> sin) * mag)


x2 : Plotter
x2 settings ticks =
    (ticks / 8 |> sin) * 50 + ((ticks / 12 |> sin) * 50)


y2 : Plotter
y2 settings ticks =
    (ticks / 8 |> cos) * 80



-- End Gen Garden
-- -----------------------------------------------------------------
-- -----------------------------------------------------------------


type alias Model =
    { garden : GenGarden.Model
    }


type Msg
    = GardenMsg GenGarden.Msg


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { garden = GenGarden.init (1000 / 30) mySliders
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GardenMsg gMsg ->
            let
                ( gModel, cmd ) =
                    GenGarden.update gMsg model.garden
            in
            ( { model | garden = gModel }
            , Cmd.batch
                [ Cmd.map GardenMsg cmd
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ GenGarden.subscriptions model.garden
            |> Sub.map GardenMsg
        ]


view : Model -> Document Msg
view model =
    { title = "Gen Garden - Flying Lines"
    , body =
        GenGarden.view drawFrame model.garden
            |> List.map (Html.map GardenMsg)
    }
