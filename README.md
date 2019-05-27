# gen-garden

A generative art framework written in Elm 0.19

`GenGarden` will render a user-provided draw function and display sliders to change user-provided variables to the draw function.  


## Adding GenGarden to an app

`GenGarden` provides `init`, `subscriptions`, `update`, and `view` functions to integrate into an app. 

```elm
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
    ( { garden = GenGarden.init 0 []
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ GenGarden.subscriptions model.garden
            |> Sub.map GardenMsg
        ]


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

view : Model -> Document Msg
view model =
    { title = "Gen Garden - Flying Lines"
    , body =
        GenGarden.view drawFrame model.garden
            |> List.map (Html.map GardenMsg)
    }
    
{-| Redraw the image area on each frame
-}
drawFrame : Dict.Dict String Float -> Float -> List (GenGarden.Drawing msg)
drawFrame _ frameNumber =
    [ GenGarden.circle ( 0, 0 ) 10 "coral [] []
    ]
```



