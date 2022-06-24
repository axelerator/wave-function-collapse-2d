module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Time
import WaveFunctionCollapse exposing (..)


type Msg
    = Pick Mode ( Int, Int ) Int
    | Step
    | Play
    | Faster
    | Slower
    | GotRandom WaveFunctionCollapse.RandomPick


type alias Model =
    { wfModel : WaveFunctionCollapse.Model Tile Socket
    , mode : Mode
    , speed : Int
    }


type Mode
    = Manual
    | AutoStep


type alias Tile =
    { filename : String
    , sockets : Sockets
    }


type alias Sockets =
    { top : Socket
    , left : Socket
    , bottom : Socket
    , right : Socket
    }


type alias Socket =
    ( TerrainType, TerrainType )


tileImages_ : List Tile
tileImages_ =
    [ fullwall
    , fullsand
    , wall_bottom
    , wall_left
    , wall_right
    , wall_top
    , wall_top_left
    , wall_top_right
    , wall_bottom_left
    , wall_bottom_right
    ]


mkTile : String -> TerrainType -> TerrainType -> TerrainType -> TerrainType -> Tile
mkTile filename t0 t1 t2 t3 =
    { filename = filename
    , sockets = mkSockets t0 t1 t2 t3
    }


mkSockets : TerrainType -> TerrainType -> TerrainType -> TerrainType -> Sockets
mkSockets t0 t1 t2 t3 =
    { top = ( t0, t3 )
    , left = ( t0, t1 )
    , bottom = ( t1, t2 )
    , right = ( t3, t2 )
    }


fullwall : Tile
fullwall =
    mkTile "full_wall.jpg" Wall Wall Wall Wall


fullsand : Tile
fullsand =
    mkTile "full_sand.jpg" Sand Sand Sand Sand


wall_left : Tile
wall_left =
    mkTile "wall_left.jpg" Wall Wall Sand Sand


wall_right : Tile
wall_right =
    mkTile "wall_right.jpg" Sand Sand Wall Wall


wall_bottom : Tile
wall_bottom =
    mkTile "wall_bottom.jpg" Sand Wall Wall Sand


wall_top : Tile
wall_top =
    mkTile "wall_top.jpg" Wall Sand Sand Wall


wall_top_left : Tile
wall_top_left =
    mkTile "wall_top_left.jpg" Wall Sand Sand Sand


wall_top_right : Tile
wall_top_right =
    mkTile "wall_top_right.jpg" Sand Sand Sand Wall


wall_bottom_left : Tile
wall_bottom_left =
    mkTile "wall_bottom_left.jpg" Sand Wall Sand Sand


wall_bottom_right : Tile
wall_bottom_right =
    mkTile "wall_bottom_right.jpg" Sand Sand Wall Sand



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


tilesDefinition : TilesDefinition Tile Socket
tilesDefinition =
    { defaultTile = fullsand
    , tiles = tileImages_
    , width = 5
    , height = 5
    , getSocketIn = getSocketIn
    }


getSocketIn : Tile -> Direction -> Socket
getSocketIn { sockets } dir =
    case dir of
        Top ->
            sockets.top

        Left ->
            sockets.left

        Bottom ->
            sockets.bottom

        Right ->
            sockets.right


init : () -> ( Model, Cmd Msg )
init _ =
    ( { wfModel = WaveFunctionCollapse.init tilesDefinition
      , mode = Manual
      , speed = 200
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick _ pos tileId ->
            ( { model | wfModel = WaveFunctionCollapse.pickTile pos tileId model.wfModel }
            , Cmd.none
            )

        Step ->
            let
                ( wfModel, cmd ) =
                    propagate GotRandom Nothing model.wfModel

                mode =
                    if WaveFunctionCollapse.done model.wfModel then
                        Manual

                    else
                        model.mode
            in
            ( { model
                | wfModel = wfModel
                , mode = mode
              }
            , cmd
            )

        Play ->
            ( { model | mode = AutoStep }
            , Cmd.none
            )

        Faster ->
            ( { model | speed = model.speed // 2 }
            , Cmd.none
            )

        Slower ->
            ( { model | speed = model.speed * 2 }
            , Cmd.none
            )

        GotRandom randomPick ->
            let
                ( wfModel, cmd ) =
                    propagate GotRandom (Just randomPick) model.wfModel
            in
            ( { model | wfModel = wfModel }
            , cmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { mode, speed } =
    case mode of
        Manual ->
            Sub.none

        AutoStep ->
            Time.every (toFloat speed) (\_ -> Step)



-- VIEW


view : Model -> Html Msg
view model =
    let
        modeView =
            case model.mode of
                AutoStep ->
                    text "Playing"

                Manual ->
                    text "Stopped"
    in
    div []
        [ div []
            [ button [ onClick <| Step ] [ text "step" ]
            , button [ onClick Play ] [ text "play ", text <| fromInt model.speed ]
            , button [ onClick Slower ] [ text "-" ]
            , button [ onClick Faster ] [ text "+" ]
            , modeView
            ]
        , viewPropGrid (Pick Manual) displayTileImage model.wfModel
        , viewTiles
        ]


displayTileImage { filename } =
    div [ style "background-image" ("url(assets/tiles/" ++ filename ++ ")") ] []


viewTiles : Html msg
viewTiles =
    let
        f i { filename } =
            div [ style "background-image" ("url(assets/tiles/" ++ filename ++ ")") ] [ text <| fromInt i, br [] [], text filename ]
    in
    div [ class "examples" ] <| List.indexedMap f tileImages_


imageForTile : Int -> String
imageForTile i =
    case List.head <| List.drop i tileImages_ of
        Nothing ->
            fullsand.filename

        Just aTile ->
            aTile.filename
