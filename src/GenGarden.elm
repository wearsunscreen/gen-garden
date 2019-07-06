module GenGarden exposing
    ( init, update, view, viewCanvas, subscriptions, Drawing, Model, Msg
    , circle, ellipse, grid, line, rect
    , Slider
    , listOfRandomInts
    )

{-| A `GenGarden` displays an image generated from a draw function passed
to it. Including settings will create sliders that well to change variables
to the draw function.


# Using GenGarden

@docs init, update, view, viewCanvas, subscriptions, Drawing, Model, Msg


# Drawing with SVG in the GenGarden

@docs circle, ellipse, grid, line, rect


# Slider

@docs Slider


# Utility functions

@docs listOfRandomInts

-}

import Canvas
import Color
import DOM
import Dict
import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode
import List exposing (length)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Random exposing (Seed, initialSeed, int, maxInt, step)
import String exposing (fromFloat, fromInt)
import Svg
import Svg.Attributes as SA
import Time exposing (Posix, millisToPosix, posixToMillis)
import Tuple exposing (first)


{-| The model for the `GenGarden`. One must be included in the app's model.
-}
type alias Model =
    { frame : Float
    , frameRate : Float
    , settings : Dict.Dict String Float
    , sliders : List Slider_Model
    , time : Posix
    }


{-| The `Msg` for the `GenGarden` used for timing frames drawing and
slider interation. The app must define one of its Msg types to
include a `GenGarden.Msg`. For example:

    type Msg
        = GardenMsg GenGarden.Msg

-}
type Msg
    = Tick Posix
    | SliderMsg1 Slider_Msg
    | SliderMsg2 Slider_Msg
    | SliderMsg3 Slider_Msg
    | SliderMsg4 Slider_Msg
    | SliderMsg5 Slider_Msg
    | SliderMsg6 Slider_Msg
    | SliderMsg7 Slider_Msg
    | SliderMsg8 Slider_Msg
    | SliderMsg9 Slider_Msg
    | SliderMsg10 Slider_Msg


allSliderMsgs model =
    let
        msgs =
            [ SliderMsg1
            , SliderMsg2
            , SliderMsg3
            , SliderMsg4
            , SliderMsg5
            , SliderMsg6
            , SliderMsg7
            , SliderMsg8
            , SliderMsg9
            , SliderMsg10
            ]
    in
    List.take (Basics.min (length model.sliders) (length msgs)) msgs


{-| The `Slider` record describes the values needing to display a slider.
A list of `Slider`s must be passed to `GenGarden.init`.
The label of the `Slider` will serve as the key to the dictionary of
current values sent to the draw functions.
-}
type alias Slider =
    { label : String
    , max : Float
    , min : Float
    , step : Float
    , value : Float
    }


getSliderAt : Int -> List Slider_Model -> Slider_Model
getSliderAt index list =
    getAt index list
        |> withDefault slider_defaultModel


defaultSetting : Slider
defaultSetting =
    { label = "dummy"
    , max = 10.0
    , min = 0.0
    , step = 0.1
    , value = 5.0
    }


{-| Replace element of a list, maintaining the order of the list
-}
replaceElement : Int -> a -> List a -> List a
replaceElement index x list =
    List.take index list ++ (x :: List.drop (index + 1) list)


{-| Initialize the GenGarden. The `frameRate` is the length of each animation
frame in milliseconds. A `frameRate` of 0 or less will only update on setting
changes.
A slider will be created for each setting in list of
settings. This example will create a GenGarden that updated once a second
and has one settings slider.

    mySliders =
    [ { label = "Number of Columns and Rows"
      , max = 60
      , min = 1
      , step = 1
      , value = 10
      }
    ]
    GenGarden.init 1000 mySliders

-}
init : Float -> List Slider -> Model
init frameRate mySliders =
    let
        initialSlider =
            slider_defaultModel

        toSlider : Slider -> Slider_Model
        toSlider setting =
            { initialSlider
                | label = setting.label
                , min = setting.min
                , max = setting.max
                , step = setting.step
                , value = setting.value
            }

        sliders =
            List.map toSlider mySliders

        toDict : List Slider -> Dict.Dict String Float
        toDict settings =
            List.map (\s -> ( s.label, s.value )) settings
                |> Dict.fromList
    in
    { frame = 0
    , frameRate = Basics.max 0 frameRate
    , settings = toDict mySliders
    , sliders = sliders
    , time = millisToPosix 0
    }


{-| Return a list of pseudo-randomly generated positive Ints. The value will be
between 0 and maxValue. An Int must be provided as a seed.
-}
listOfRandomInts : Int -> Int -> Int -> List Int
listOfRandomInts length maxValue seedInt =
    let
        listOfRandomInts_ : Int -> Int -> Random.Seed -> List ( Int, Random.Seed )
        listOfRandomInts_ count maxV seed =
            if count == 0 then
                []

            else
                let
                    ( v, seed_ ) =
                        Random.step (Random.int 0 maxV) seed
                in
                ( v, seed_ ) :: listOfRandomInts_ (count - 1) maxV seed_
    in
    listOfRandomInts_ length maxValue (Random.initialSeed seedInt)
        |> List.map Tuple.first


px : Int -> String
px v =
    fromInt v ++ "px"


updateSlider : (Slider_Msg -> Msg) -> Slider_Msg -> Int -> Model -> ( Model, Cmd Msg )
updateSlider constructor sliderMsg index model =
    let
        fromSliders : List Slider_Model -> Dict.Dict String Float
        fromSliders sliders =
            List.map (\s -> ( s.label, s.value )) sliders
                |> Dict.fromList

        ( newSlider, cmd, updateResults ) =
            slider_update sliderMsg (getSliderAt index model.sliders)

        newModel =
            { model
                | sliders = replaceElement index newSlider model.sliders
                , settings = fromSliders model.sliders
            }

        newCmd =
            if updateResults then
                Cmd.batch [ Cmd.map constructor cmd, Cmd.none ]

            else
                Cmd.none
    in
    ( newModel, newCmd )


{-| Updates the `GenGarden` model. Example of usage:

    type Msg
        = GardenMsg GenGarden.Msg

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

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { model | time = t, frame = model.frame + 1 }, Cmd.none )

        SliderMsg1 sliderMsg ->
            updateSlider SliderMsg1 sliderMsg 0 model

        SliderMsg2 sliderMsg ->
            updateSlider SliderMsg2 sliderMsg 1 model

        SliderMsg3 sliderMsg ->
            updateSlider SliderMsg3 sliderMsg 2 model

        SliderMsg4 sliderMsg ->
            updateSlider SliderMsg4 sliderMsg 3 model

        SliderMsg5 sliderMsg ->
            updateSlider SliderMsg5 sliderMsg 4 model

        SliderMsg6 sliderMsg ->
            updateSlider SliderMsg6 sliderMsg 5 model

        SliderMsg7 sliderMsg ->
            updateSlider SliderMsg7 sliderMsg 6 model

        SliderMsg8 sliderMsg ->
            updateSlider SliderMsg8 sliderMsg 7 model

        SliderMsg9 sliderMsg ->
            updateSlider SliderMsg9 sliderMsg 8 model

        SliderMsg10 sliderMsg ->
            updateSlider SliderMsg10 sliderMsg 9 model


{-| Set the subscriptions for `GenGarden`. Example of usage:

    type Msg
        = GardenMsg GenGarden.Msg

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Sub.batch
            [ Sub.map GardenMsg <| GenGarden.subscriptions model.garden
            ]

-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        fr =
            if model.frameRate > 0 then
                [ Time.every model.frameRate Tick ]

            else
                []

        build : Slider_Model -> (Slider_Msg -> Msg) -> Sub Msg
        build slider constructor =
            Sub.map constructor <| slider_subscriptions slider
    in
    Sub.batch
        (List.map2
            build
            model.sliders
            (allSliderMsgs model)
            ++ fr
        )


{-| Displays the GenGarden in SVG. Provide the drawing function in `drawFrame`,
and the `GenGarden.Model` in 'model\`. Example of usage:

    type Msg
        = GardenMsg GenGarden.Msg

    view : Model -> Document Msg
    view model =
        { title = "Gen Garden"
        , body =
            List.map (Html.map GardenMsg) <|
                GenGarden.view drawFrame model.garden
        }

-}
view :
    (Dict.Dict String Float -> Float -> List (Drawing Msg))
    -> Model
    -> List (Html Msg)
view drawFrame model =
    let
        f slider constructor =
            slider_view slider |> Html.map constructor
    in
    viewSvg drawFrame model :: List.map2 f model.sliders (allSliderMsgs model)


canvasWidth =
    200


canvasHeight =
    canvasWidth


{-| Displays the GenGarden on an HTML Canvas. Provide the drawing function in
`drawFrame`, and the `GenGarden.Model` in `model`. Example of usage:

    view : Model -> Document Msg
    view model =
        { title = "Gen Garden"
        , body =
            GenGarden.viewCanvas drawFrame model.garden
                |> List.map (Html.map GardenMsg)
        }

-}
viewCanvas :
    (Dict.Dict String Float -> Float -> List Canvas.Renderable)
    -> Model
    -> List (Html Msg)
viewCanvas drawFrame model =
    let
        f slider constructor =
            slider_view slider |> Html.map constructor
    in
    Canvas.toHtml ( canvasWidth, canvasHeight )
        []
        (Canvas.shapes
            [ Canvas.fill Color.white, Canvas.stroke <| Color.rgb255 0 53 155 ]
            [ Canvas.rect ( 0, 0 ) canvasWidth canvasHeight ]
            :: drawFrame model.settings model.frame
        )
        :: List.map2 f model.sliders (allSliderMsgs model)


viewSvg :
    (Dict.Dict String Float -> Float -> List (Drawing Msg))
    -> Model
    -> Html Msg
viewSvg drawFrame model =
    Svg.svg (viewBox 0 0 canvasWidth canvasWidth 600) <|
        [ Svg.g
            [ SA.transform <|
                "translate("
                    ++ fromFloat (canvasWidth / 2)
                    ++ ","
                    ++ fromFloat (canvasWidth / 2)
                    ++ ")"
            ]
            (drawFrame model.settings model.frame)
        ]


viewBox : Float -> Float -> Float -> Float -> Int -> List (Html.Attribute msg)
viewBox l r w h vpw =
    [ SA.viewBox <| fromFloat l ++ " " ++ fromFloat r ++ " " ++ fromFloat w ++ " " ++ fromFloat h
    , SA.width <| px vpw
    ]



--
-- Svg utilities
--


{-| Drawing functions return a `Drawing`.
-}
type alias Drawing msg =
    Svg.Svg msg


{-| Draw a circle. Example of usage:

    type Msg
        = GardenMsg GenGarden.Msg

    drawFrame : List GenGarden.Slider -> Float -> List (GenGarden.Drawing msg)
    drawFrame settings frame =
        GenGarden.circle ( -5, -5 ) 20 "red" [] []
            :: GenGarden.grid

-}
circle :
    ( Float, Float )
    -> Float
    -> String
    -> List (Html.Attribute msg)
    -> List (Drawing msg)
    -> Drawing msg
circle ( cx, cy ) radius color attributes children =
    Svg.circle
        (attributes ++ [ SA.cx <| fromFloat cx, SA.cy <| fromFloat cy, SA.r <| fromFloat radius, SA.fill color ])
        children


{-| Draw an ellipse. Example of usage:

    type Msg
        = GardenMsg GenGarden.Msg

    drawFrame : List GenGarden.Slider -> Float -> List (GenGarden.Drawing msg)
    drawFrame settings frame =
        GenGarden.ellipse ( -5, -5 ) 4 8 "red" [] []
            :: GenGarden.grid

-}
ellipse :
    ( Float, Float )
    -> Float
    -> Float
    -> String
    -> List (Html.Attribute msg)
    -> List (Drawing msg)
    -> Drawing msg
ellipse ( cx, cy ) xRadius yRadius color attributes children =
    Svg.ellipse
        (attributes
            ++ [ SA.cx <| fromFloat cx
               , SA.cy <| fromFloat cy
               , SA.rx <| fromFloat xRadius
               , SA.ry <| fromFloat yRadius
               , SA.fill color
               ]
        )
        children


{-| Draw the coordinate grid. Example of usage:

    type Msg
        = GardenMsg GenGarden.Msg

    drawFrame : List GenGarden.Slider -> Float -> List (GenGarden.Drawing msg)
    drawFrame settings frame =
        GenGarden.circle ( -5, -5 ) 20 "red" [] []
            :: GenGarden.grid

-}
grid : List (Drawing msg)
grid =
    let
        attrs =
            [ SA.strokeWidth <| fromFloat 0.5 ]

        half =
            canvasWidth / 2

        c =
            "rgba(180,180,185,0.5)"
    in
    [ line ( -half, 0 ) ( half, 0 ) c attrs []
    , line ( 0, -half ) ( 0, half ) c attrs []
    ]


{-| Draw a line. Example of usage:

    type Msg
        = GardenMsg GenGarden.Msg

    drawFrame : List GenGarden.Slider -> Float -> List (GenGarden.Drawing msg)
    drawFrame settings frame =
        [ GenGarden.line ( -5, -5 ) ( 20, 15 ) "red" [] [] ]

-}
line :
    ( Float, Float )
    -> ( Float, Float )
    -> String
    -> List (Html.Attribute msg)
    -> List (Drawing msg)
    -> Drawing msg
line ( xa, ya ) ( xb, yb ) color attributes children =
    Svg.line
        (attributes
            ++ [ SA.x1 <| fromFloat xa
               , SA.y1 <| fromFloat ya
               , SA.x2 <| fromFloat xb
               , SA.y2 <| fromFloat yb
               , SA.stroke color
               ]
        )
        children


{-| Draw a rect. Example of usage:

    type Msg
        = GardenMsg GenGarden.Msg

    drawFrame : List GenGarden.Slider -> Float -> List (GenGarden.Drawing msg)
    drawFrame settings frame =
        [ GenGarden.rect (( -5, -5 ) ( 20, 15 )) "red" [] [] ]

-}
rect :
    ( ( Float, Float ), ( Float, Float ) )
    -> String
    -> List (Html.Attribute msg)
    -> List (Drawing msg)
    -> Drawing msg
rect ( ( rx, ry ), ( width, height ) ) color attributes children =
    Svg.rect
        (attributes
            ++ [ SA.x <| fromFloat rx
               , SA.y <| fromFloat ry
               , SA.width <| fromFloat width
               , SA.height <| fromFloat height
               , SA.fill color
               ]
        )
        children



-- ----------------------------------------------------------------------
-- Begin Slider implementation, borrowing heavily from
-- https://github.com/carwow/elm-slider/
--


{-| The base model for the slider
-}
type alias Slider_Model =
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    , minFormatter : Float -> String
    , maxFormatter : Float -> String
    , currentValueFormatter : Float -> Float -> String
    , disabled : Bool
    , progressDirection : ProgressDirection
    , label : String
    }


{-| The basic type accepted by the update
-}
type Slider_Msg
    = TrackClicked String
    | RangeChanged String Bool


{-| Progress Bar direction (left or right)
-}
type ProgressDirection
    = ProgressLeft
    | ProgressRight


{-| Default model
-}
slider_defaultModel : Slider_Model
slider_defaultModel =
    { min = 0
    , max = 100
    , step = 10
    , value = 0
    , minFormatter = String.fromFloat
    , maxFormatter = String.fromFloat
    , currentValueFormatter = defaultCurrentValueFormatter
    , disabled = False
    , progressDirection = ProgressLeft
    , label = "."
    }


{-| Default formatter for the current value
-}
defaultCurrentValueFormatter : Float -> Float -> String
defaultCurrentValueFormatter currentValue max =
    if currentValue == max then
        ""

    else
        String.fromFloat currentValue


{-| takes a model and a message and applies it to create an updated model
-}
slider_update : Slider_Msg -> Slider_Model -> ( Slider_Model, Cmd Slider_Msg, Bool )
slider_update message model =
    case message of
        RangeChanged newValue shouldFetchModels ->
            let
                convertedValue =
                    String.toFloat newValue |> Maybe.withDefault 0

                newModel =
                    { model | value = convertedValue }
            in
            ( newModel, Cmd.none, shouldFetchModels )

        TrackClicked newValue ->
            let
                convertedValue =
                    snapValue (String.toFloat newValue |> Maybe.withDefault model.min) model

                newModel =
                    { model | value = convertedValue }
            in
            ( newModel, Cmd.none, True )


closestStep : Float -> Float -> Int
closestStep value step =
    let
        roundedValue =
            round value

        roundedStep =
            if round step > 0 then
                round step

            else
                1

        remainder =
            remainderBy roundedStep roundedValue
    in
    if remainder > (roundedStep // 2) then
        (roundedValue - remainder) + roundedStep

    else
        roundedValue - remainder


snapValue : Float -> Slider_Model -> Float
snapValue value model =
    let
        roundedStep =
            round model.step

        adjustedRoundedStep =
            if roundedStep > 0 then
                roundedStep

            else
                1

        newValue =
            value / toFloat adjustedRoundedStep

        roundedValue =
            case model.progressDirection of
                ProgressLeft ->
                    floor newValue

                ProgressRight ->
                    ceiling newValue

        nextValue =
            toFloat (roundedValue * adjustedRoundedStep)
    in
    nextValue


onOutsideRangeClick : Slider_Model -> Json.Decode.Decoder Slider_Msg
onOutsideRangeClick model =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        clickedValue =
                            (((model.max - model.min) / rectangle.width) * mouseX) + model.min

                        newValue =
                            closestStep clickedValue model.step
                    in
                    String.fromInt newValue
                )
                (Json.Decode.at [ "target" ] DOM.boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map TrackClicked valueDecoder


onInsideRangeClick : Slider_Model -> Json.Decode.Decoder Slider_Msg
onInsideRangeClick model =
    let
        valueDecoder =
            Json.Decode.map2
                (\rectangle mouseX ->
                    let
                        adjustedValue =
                            clamp model.min model.max model.value

                        newValue =
                            round <|
                                case model.progressDirection of
                                    ProgressLeft ->
                                        (adjustedValue / rectangle.width) * mouseX

                                    ProgressRight ->
                                        adjustedValue + ((mouseX / rectangle.width) * (model.max - adjustedValue))

                        adjustedNewValue =
                            clamp model.min model.max <| toFloat newValue
                    in
                    String.fromFloat adjustedNewValue
                )
                (Json.Decode.at [ "target" ] DOM.boundingClientRect)
                (Json.Decode.at [ "offsetX" ] Json.Decode.float)
    in
    Json.Decode.map TrackClicked valueDecoder


onRangeChange : Bool -> Json.Decode.Decoder Slider_Msg
onRangeChange shouldFetchModels =
    Json.Decode.map2
        RangeChanged
        targetValue
        (Json.Decode.succeed shouldFetchModels)


labelAttributes : List (Html.Attribute Slider_Msg)
labelAttributes =
    [ style "color" "black"
    , style "font-size" "130%"
    , style "color" "green"
    ]


{-| Displays the slider
-}
slider_view : Slider_Model -> Html Slider_Msg
slider_view model =
    let
        trackAttributes =
            [ Html.Attributes.class "input-range__track" ]

        trackAllAttributes =
            case model.disabled of
                False ->
                    List.append trackAttributes [ Html.Events.on "click" (onOutsideRangeClick model) ]

                True ->
                    trackAttributes

        progressPercentages =
            calculateProgressPercentages model

        progressAttributes =
            [ Html.Attributes.class "input-range__progress"
            , Html.Attributes.style "left" <| String.fromFloat progressPercentages.left ++ "%"
            , Html.Attributes.style "right" <| String.fromFloat progressPercentages.right ++ "%"
            ]

        progressAllAttributes =
            case model.disabled of
                False ->
                    List.append progressAttributes [ Html.Events.on "click" (onInsideRangeClick model) ]

                True ->
                    progressAttributes
    in
    div []
        [ div
            labelAttributes
            [ div [ Html.Attributes.class "input-range-label" ] [ Html.text model.label ]
            ]
        , div
            [ Html.Attributes.class "input-range-container" ]
            [ Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min (String.fromFloat model.min)
                , Html.Attributes.max (String.fromFloat model.max)
                , Html.Attributes.value <| String.fromFloat model.value
                , Html.Attributes.step (String.fromFloat model.step)
                , Html.Attributes.class "input-range"
                , Html.Attributes.disabled model.disabled
                , Html.Events.on "change" (onRangeChange True)
                , Html.Events.on "input" (onRangeChange False)
                ]
                []
            , div
                trackAllAttributes
                []
            , div
                progressAllAttributes
                []
            ]
        , div
            []
            [ div []
                [ Html.text (model.currentValueFormatter model.value model.max) ]
            ]
        ]


{-| Returns the percentage adjusted min, max values for the range (actual min - actual max)
-}
calculateProgressPercentages : Slider_Model -> { left : Float, right : Float }
calculateProgressPercentages model =
    let
        progressRatio =
            100 / (model.max - model.min)

        value =
            clamp model.min model.max model.value
    in
    case model.progressDirection of
        ProgressRight ->
            { left = (value - model.min) * progressRatio, right = 0.0 }

        ProgressLeft ->
            { left = 0.0, right = (model.max - value) * progressRatio }



-- Subscriptions ---------------------------------------------------------------


{-| Returns the subscriptions necessary to run
-}
slider_subscriptions : Slider_Model -> Sub Slider_Msg
slider_subscriptions model =
    Sub.none



{--End Slider implementation
--}
