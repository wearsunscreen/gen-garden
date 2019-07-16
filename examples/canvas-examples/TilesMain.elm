module Main exposing (init, main, view)

import Browser
import Canvas
import Color
import Dict
import GenGarden
import Html
import Html.Attributes exposing (style)
import List.Extra
import Maybe
import Random



-- -----------------------------------------------------------------
-- -----------------------------------------------------------------
-- Start Gen Garden


{-| Each Slider record will generate a slider to allow you to change a
-- draw setting at run time. You can have up to 10 sliders.
-}
mySliders : List GenGarden.Slider
mySliders =
    [ { label = "Number of Columns and Rows"
      , max = 60
      , min = 1
      , step = 1
      , value = 10
      }
    ]


{-| Redraw the image area, pass this to GenGarden.view
-}
drawFrameTiles : Dict.Dict String Float -> Float -> List Canvas.Renderable
drawFrameTiles settings ticks =
    let
        numCols =
            Dict.get "Number of Columns and Rows" settings
                |> Maybe.withDefault 10
                |> round

        wide =
            200 / toFloat numCols

        -- set up tiles from 0,0 to numCols,numCols
        tilePos =
            List.range 0 (numCols * numCols)
                |> List.map
                    (\tileIndex -> ( modBy numCols tileIndex, tileIndex // numCols ))

        tilePosBool =
            round ticks
                |> GenGarden.listOfRandomInts (numCols * numCols) 1
                |> List.map (\x -> x == 0)
                |> List.Extra.zip tilePos

        tile : ( ( Int, Int ), Bool ) -> List Canvas.Shape
        tile ( ( x, y ), b ) =
            drawTile ( x, y ) b wide "red" settings
    in
    [ Canvas.shapes [ Canvas.fill Color.white, Canvas.stroke <| Color.rgb255 0 53 155 ]
        (Canvas.rect
            ( 0, 0 )
            width
            height
            :: List.concatMap
                (\x -> tile x)
                tilePosBool
        )
    ]


{-| draw two lines from the midpoints of the perimeter of the tile. `flip` will
determine if lines slant left or right.
-}
drawTile :
    ( Int, Int )
    -> Bool
    -> Float
    -> String
    -> Dict.Dict String Float
    -> List Canvas.Shape
drawTile ( tx, ty ) flip wide color settings =
    let
        x =
            toFloat tx

        y =
            toFloat ty

        left =
            x * wide

        top =
            y * wide

        half =
            wide / 2

        leftCenter =
            ( left, top + half )

        rightCenter =
            ( left + wide, top + half )

        topCenter =
            ( left + half, top )

        bottomCenter =
            ( left + half, top + wide )

        ( ( start, end ), ( start2, end2 ) ) =
            if flip then
                ( ( leftCenter, topCenter ), ( rightCenter, bottomCenter ) )

            else
                ( ( rightCenter, topCenter ), ( leftCenter, bottomCenter ) )
    in
    [ Canvas.path start
        [ Canvas.lineTo end ]
    , Canvas.path start2
        [ Canvas.lineTo end2 ]
    ]


height : number
height =
    400


width : number
width =
    720



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
    ( { garden = GenGarden.init 1000 mySliders
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


view : Model -> Html.Html Msg
view model =
    Html.div []
        (GenGarden.viewCanvas drawFrameTiles model.garden
            |> List.map (Html.map GardenMsg)
        )
