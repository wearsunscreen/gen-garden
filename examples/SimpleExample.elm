module Main exposing (init, main, view)

import Browser exposing (Document, document)
import Dict
import GenGarden
import Html
import Maybe exposing (withDefault)



-- -----------------------------------------------------------------
-- -----------------------------------------------------------------
-- Start Gen Garden


{-| Each Slider record will generate a slider to allow you to change a
-- draw setting at run time. You are limited to 10 sliders.
-}
mySliders : List GenGarden.Slider
mySliders =
    [ { label = "Length of lines"
      , max = 100
      , min = 1
      , step = 1
      , value = 50
      }
    , { label = "Circle radius"
      , max = 99
      , min = 0
      , step = 1
      , value = 50
      }
    , { label = "Circle alpha"
      , max = 1.0
      , min = 0.0
      , step = 0.05
      , value = 0.7
      }
    ]


{-| Redraw the image area on each frame
-}
drawFrame : Dict.Dict String Float -> Float -> List (GenGarden.Drawing msg)
drawFrame settings frameNumber =
    let
        x =
            Dict.get "Length of lines" settings |> withDefault 50

        r =
            Dict.get "Circle radius" settings |> withDefault 30

        color =
            "rgba(50,250,50,"
                ++ String.fromFloat (Dict.get "Circle alpha" settings |> withDefault 0.5)
                ++ ")"
    in
    [ GenGarden.line ( x * -1, x * -1 ) ( x, x ) "red" [] []
    , GenGarden.circle ( 0, 0 ) r color [] []
    , GenGarden.line ( x * -1, x ) ( x, x * -1 ) "coral" [] []
    ]



-- End Gen Garden
-- -----------------------------------------------------------------
-- -----------------------------------------------------------------


type alias Model =
    { garden : GenGarden.Model
    }


type Msg
    = GardenMsg GenGarden.Msg


main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { garden = GenGarden.init 0 mySliders
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
    { title = "Gen Garden - Simple Example"
    , body =
        GenGarden.view drawFrame model.garden
            |> List.map (Html.map GardenMsg)
    }
