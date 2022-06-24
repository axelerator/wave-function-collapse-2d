module WaveFunctionCollapse exposing
    ( Model(..)
    , ModelDetails
    , PropagationTile(..)
    , RandomPick
    , done
    , imageForTile
    , init
    , pickTile
    , propagate
    , stopped
    , tileById
    , tileImages_
    )

import Grid exposing (Grid)
import Html exposing (option)
import Html.Events exposing (custom)
import Random


type Model
    = Model ModelDetails


pickTile : Pos -> TileId -> Model -> Model
pickTile pos tileId (Model model) =
    Model
        { model
            | openSteps = PickTile pos tileId :: model.openSteps
        }


stopped : Model -> Bool
stopped (Model { openSteps }) =
    List.isEmpty openSteps


done : Model -> Bool
done (Model { propGrid }) =
    let
        f t onlyFixedTiles =
            case t of
                Superposition _ ->
                    False

                Fixed _ ->
                    onlyFixedTiles
    in
    Grid.foldr f True propGrid


propagate : (RandomPick -> msg) -> Maybe RandomPick -> Model -> ( Model, Cmd msg )
propagate requestRandom maybeRandom ((Model modelDetails) as model) =
    case modelDetails.openSteps of
        step :: otherSteps ->
            let
                ( additionalSteps, nextGrid ) =
                    processStep modelDetails step modelDetails.propGrid
            in
            ( Model
                { modelDetails
                    | propGrid = nextGrid
                    , openSteps = otherSteps ++ additionalSteps
                }
            , Cmd.none
            )

        [] ->
            if not (done model) then
                case maybeRandom of
                    Just randomPick ->
                        ( Model <| pickRandom randomPick modelDetails
                        , Cmd.none
                        )

                    Nothing ->
                        ( model
                        , mkRandom requestRandom modelDetails
                        )

            else
                ( model, Cmd.none )


init : Int -> Int -> Model
init w h =
    Model
        { propGrid = Grid.repeat w h (superposition tileImages_)
        , openSteps = []
        , tileImages = tileImages_
        }


type alias ModelDetails =
    { propGrid : PropagationGrid
    , openSteps : List PropStep
    , tileImages : List TileImage
    }


type TerrainType
    = Sand
    | Wall


type alias TileImage =
    { filename : String
    , sockets : Sockets
    }


tileImages_ : List TileImage
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


superposition : List TileImage -> PropagationTile
superposition tileImages =
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


canDock : ModelDetails -> Direction -> Socket -> Int -> Bool
canDock modelDetails dockDir dockSocket dockTileId =
    let
        dockTile =
            tileById modelDetails dockTileId

        currentSocket =
            getSocketIn dockTile dockDir
    in
    currentSocket == dockSocket


type alias Candidate =
    { pos : Pos
    , options : List TileId
    }


nextCandidates : ModelDetails -> List Candidate
nextCandidates { propGrid, tileImages } =
    let
        gridWithIdx =
            Grid.indexedMap (\x y t -> ( x, y, t )) propGrid

        f ( x, y, t ) ( candidates, length ) =
            case t of
                Fixed _ ->
                    ( candidates, length )

                Superposition options ->
                    let
                        currentLength =
                            List.length options
                    in
                    if currentLength > length then
                        ( candidates, length )

                    else if currentLength < length then
                        ( [ { pos = ( x, y ), options = options } ]
                        , currentLength
                        )

                    else
                        ( { pos = ( x, y ), options = options } :: candidates
                        , currentLength
                        )
    in
    Tuple.first <| Grid.foldr f ( [], List.length tileImages ) gridWithIdx


pickRandom : RandomPick -> ModelDetails -> ModelDetails
pickRandom (RandomPick ( posRand, tileRand )) modelDetails =
    let
        candidates =
            nextCandidates modelDetails

        pickRandomStep =
            if List.isEmpty candidates then
                []

            else
                let
                    randomCandidate =
                        List.head <| List.drop (modBy (List.length candidates) posRand) candidates
                in
                case randomCandidate of
                    Just { pos, options } ->
                        let
                            randomTileId =
                                List.head <| List.drop (modBy (List.length options) tileRand) options
                        in
                        case randomTileId of
                            Just tileId ->
                                [ PickTile pos tileId ]

                            Nothing ->
                                []

                    Nothing ->
                        []
    in
    { modelDetails
        | openSteps = pickRandomStep ++ modelDetails.openSteps
    }


processStep : ModelDetails -> PropStep -> PropagationGrid -> ( List PropStep, PropagationGrid )
processStep modelDetails step grid =
    case step of
        PickTile pos tileId ->
            let
                mkStep dir =
                    KeepOnlyMatching pos (posInDir dir pos)

                nextSteps =
                    List.map mkStep allDirections
            in
            ( nextSteps
            , Grid.set pos (Fixed tileId) grid
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
                            tileById modelDetails originTileId

                        originSocket =
                            getSocketIn originTile dir

                        revisedOptions =
                            List.filter (canDock modelDetails (invert dir) originSocket) options

                        updateGrid =
                            Grid.set to (Superposition revisedOptions) grid
                    in
                    ( [], updateGrid )

                _ ->
                    ( [], grid )


type RandomPick
    = RandomPick ( Int, Int )


randomTileAndTileIdGen : ModelDetails -> Random.Generator ( Int, Int )
randomTileAndTileIdGen { propGrid, tileImages } =
    let
        tileCount =
            Grid.width propGrid * Grid.height propGrid
    in
    Random.pair (Random.int 0 tileCount) (Random.int 0 (List.length tileImages))


mkRandom : (RandomPick -> msg) -> ModelDetails -> Cmd msg
mkRandom mkMsg modelDetails =
    Random.generate (\numbers -> mkMsg (RandomPick numbers)) (randomTileAndTileIdGen modelDetails)


imageForTile : Model -> Int -> String
imageForTile (Model { tileImages }) i =
    case List.head <| List.drop i tileImages of
        Nothing ->
            fullsand.filename

        Just aTile ->
            aTile.filename


tileById : ModelDetails -> Int -> TileImage
tileById { tileImages } i =
    case List.head <| List.drop i tileImages of
        Nothing ->
            fullsand

        Just aTile ->
            aTile
