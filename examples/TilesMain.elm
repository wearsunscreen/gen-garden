module Main exposing (init, main, view)

import Browser exposing (Document, document)
import DOM exposing (boundingClientRect)
import Dict
import GenGarden
import Html
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


{-| Redraw the image area, called every tick
-}
drawFrame : Dict.Dict String Float -> Float -> List (GenGarden.Drawing msg)
drawFrame settings ticks =
    let
        numCols =
            Dict.get "Number of Columns and Rows" settings
                |> Maybe.withDefault 10
                |> round

        width =
            200 / toFloat numCols

        -- set up tiles from 0,0 to numCols,numCols
        tilePos =
            List.range 0 (numCols * numCols)
                |> List.map
                    (\tileIndex -> ( modBy numCols tileIndex, tileIndex // numCols ))

        tilePosBool =
            round ticks
                |> listOfRandomInts (numCols * numCols)
                |> List.map (\x -> modBy 2 x == 1)
                |> List.Extra.zip tilePos

        tile : ( ( Int, Int ), Bool ) -> List (GenGarden.Drawing msg)
        tile ( ( x, y ), b ) =
            drawTile2 ( x, y ) b width "purple" settings
    in
    List.concatMap
        (\x -> tile x)
        tilePosBool


{-| Redraw the image area, called every tick
-}
drawTile2 :
    ( Int, Int )
    -> Bool
    -> Float
    -> String
    -> Dict.Dict String Float
    -> List (GenGarden.Drawing msg)
drawTile2 ( tx, ty ) flip width color settings =
    let
        x =
            toFloat tx

        y =
            toFloat ty

        left =
            x * width - 100

        top =
            y * width - 100

        half =
            width / 2

        leftCenter =
            ( left, top + half )

        rightCenter =
            ( left + width, top + half )

        topCenter =
            ( left + half, top )

        bottomCenter =
            ( left + half, top + width )

        ( ( start, end ), ( start2, end2 ) ) =
            if flip then
                ( ( leftCenter, topCenter ), ( rightCenter, bottomCenter ) )

            else
                ( ( rightCenter, topCenter ), ( leftCenter, bottomCenter ) )
    in
    [ GenGarden.line start end color [] []
    , GenGarden.line start2 end2 color [] []
    ]


{-| return a list of randomly generated positive Ints
-}
listOfRandomInts : Int -> Int -> List Int
listOfRandomInts length seedInt =
    let
        listOfRandomInts_ : Int -> Random.Seed -> List ( Int, Random.Seed )
        listOfRandomInts_ count seed =
            if count == 0 then
                []

            else
                let
                    ( v, seed_ ) =
                        Random.step (Random.int 0 Random.maxInt) seed
                in
                ( v, seed_ ) :: listOfRandomInts_ (count - 1) seed_
    in
    listOfRandomInts_ length (Random.initialSeed seedInt)
        |> List.map Tuple.first



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


view : Model -> Document Msg
view model =
    { title = "Gen Garden - Tiles Example"
    , body =
        GenGarden.view drawFrame model.garden
            |> List.map (Html.map GardenMsg)
    }
