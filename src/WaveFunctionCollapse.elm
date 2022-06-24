module WaveFunctionCollapse exposing (..)

import Grid exposing (Grid)
import Random


type alias Model =
    { propGrid : PropagationGrid
    , openSteps : List PropStep
    , mode : Mode
    , speed : Int
    }


type Msg
    = Pick Mode ( Int, Int ) Int
    | Step
    | Play
    | Faster
    | Slower


type Mode
    = Manual
    | AutoStep


type TerrainType
    = Sand
    | Wall


type alias TileImage =
    { filename : String
    , sockets : Sockets
    }


tileImages : List TileImage
tileImages =
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


type Direction
    = Top
    | Left
    | Bottom
    | Right


type alias Socket =
    ( TerrainType, TerrainType )


type alias Sockets =
    { top : Socket
    , left : Socket
    , bottom : Socket
    , right : Socket
    }


mkTile : String -> TerrainType -> TerrainType -> TerrainType -> TerrainType -> TileImage
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


fullwall : TileImage
fullwall =
    mkTile "full_wall.jpg" Wall Wall Wall Wall


fullsand : TileImage
fullsand =
    mkTile "full_sand.jpg" Sand Sand Sand Sand


wall_left : TileImage
wall_left =
    mkTile "wall_left.jpg" Wall Wall Sand Sand


wall_right : TileImage
wall_right =
    mkTile "wall_right.jpg" Sand Sand Wall Wall


wall_bottom : TileImage
wall_bottom =
    mkTile "wall_bottom.jpg" Sand Wall Wall Sand


wall_top : TileImage
wall_top =
    mkTile "wall_top.jpg" Wall Sand Sand Wall


wall_top_left : TileImage
wall_top_left =
    mkTile "wall_top_left.jpg" Wall Sand Sand Sand


wall_top_right : TileImage
wall_top_right =
    mkTile "wall_top_right.jpg" Sand Sand Sand Wall


wall_bottom_left : TileImage
wall_bottom_left =
    mkTile "wall_bottom_left.jpg" Sand Wall Sand Sand


wall_bottom_right : TileImage
wall_bottom_right =
    mkTile "wall_bottom_right.jpg" Sand Sand Wall Sand


type PropagationTile
    = Fixed Int
    | Superposition (List Int)


type alias PropagationGrid =
    Grid PropagationTile


type alias TileId =
    Int


type alias Pos =
    ( Int, Int )


type PropStep
    = PickTile Pos TileId
    | KeepOnlyMatching Pos Pos
    | ChooseRandom


superposition : PropagationTile
superposition =
    Superposition <| List.range 0 (List.length tileImages)


findDirection : ( comparable, comparable ) -> ( comparable, comparable ) -> Direction
findDirection ( x0, y0 ) ( x1, y1 ) =
    if x1 > x0 then
        Right

    else if y1 > y0 then
        Bottom

    else if y0 > y1 then
        Top

    else
        Left


getSocketIn : TileImage -> Direction -> Socket
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


posInDir : Direction -> Pos -> Pos
posInDir dir ( x, y ) =
    case dir of
        Top ->
            ( x, y - 1 )

        Bottom ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


invert : Direction -> Direction
invert dir =
    case dir of
        Top ->
            Bottom

        Left ->
            Right

        Bottom ->
            Top

        Right ->
            Left


allDirections : List Direction
allDirections =
    [ Top, Left, Bottom, Right ]


canDock : Direction -> Socket -> Int -> Bool
canDock dockDir dockSocket dockTileId =
    let
        dockTile =
            tileById dockTileId

        currentSocket =
            getSocketIn dockTile dockDir
    in
    currentSocket == dockSocket


processStep : Mode -> PropStep -> PropagationGrid -> ( List PropStep, PropagationGrid, Cmd Msg )
processStep mode step grid =
    case step of
        ChooseRandom ->
            let
                gridWithIdx =
                    Grid.indexedMap (\x y t -> ( x, y, t )) grid

                f ( x, y, t ) best =
                    case t of
                        Fixed _ ->
                            best

                        Superposition options ->
                            let
                                currentLength =
                                    List.length options
                            in
                            case best of
                                Nothing ->
                                    Just ( x, y, currentLength )

                                Just (( _, _, bestLength ) as prev) ->
                                    if currentLength < bestLength then
                                        Just ( x, y, currentLength )

                                    else
                                        Just prev

                bestNextTile =
                    Grid.foldr f Nothing gridWithIdx

                maybePickRandomTile : Cmd Msg
                maybePickRandomTile =
                    case bestNextTile of
                        Just ( x, y, optionCount ) ->
                            let
                                tile =
                                    Grid.get ( x, y ) grid
                            in
                            case tile of
                                Just (Superposition options) ->
                                    case List.head options of
                                        Just tileId ->
                                            let
                                                max =
                                                    List.length options - 1

                                                pickFromRandom i =
                                                    case List.head <| List.drop i options of
                                                        Just nextTileId ->
                                                            Pick mode ( x, y ) nextTileId

                                                        Nothing ->
                                                            Step

                                                fromOptionIdx =
                                                    Random.int 0 max
                                            in
                                            Random.generate pickFromRandom fromOptionIdx

                                        Nothing ->
                                            Cmd.none

                                _ ->
                                    Cmd.none

                        _ ->
                            Cmd.none
            in
            ( []
            , grid
            , maybePickRandomTile
            )

        PickTile pos tileId ->
            let
                mkStep dir =
                    KeepOnlyMatching pos (posInDir dir pos)

                nextSteps =
                    List.map mkStep allDirections

                autoProgress =
                    case mode of
                        AutoStep ->
                            [ ChooseRandom ]

                        _ ->
                            []
            in
            ( nextSteps ++ autoProgress
            , Grid.set pos (Fixed tileId) grid
            , Cmd.none
            )

        KeepOnlyMatching from to ->
            let
                originTile_ =
                    Grid.get from grid

                targetTile_ =
                    Grid.get to grid
            in
            case ( originTile_, targetTile_ ) of
                ( Just (Fixed originTileId), Just (Superposition options) ) ->
                    let
                        dir =
                            findDirection from to

                        originTile =
                            tileById originTileId

                        originSocket =
                            getSocketIn originTile dir

                        revisedOptions =
                            List.filter (canDock (invert dir) originSocket) options

                        updateGrid =
                            Grid.set to (Superposition revisedOptions) grid
                    in
                    ( [], updateGrid, Cmd.none )

                _ ->
                    ( [], grid, Cmd.none )


propagate : Model -> ( Model, Cmd Msg )
propagate model =
    case model.openSteps of
        step :: otherSteps ->
            let
                ( additionalSteps, nextGrid, cmd ) =
                    processStep model.mode step model.propGrid
            in
            ( { model
                | propGrid = nextGrid
                , openSteps = otherSteps ++ additionalSteps
              }
            , cmd
            )

        _ ->
            ( { model | mode = Manual }
            , Cmd.none
            )


propGrid : Int -> Int -> PropagationGrid
propGrid w h =
    Grid.repeat w h superposition


tileById : Int -> TileImage
tileById i =
    case List.head <| List.drop i tileImages of
        Nothing ->
            fullsand

        Just aTile ->
            aTile
