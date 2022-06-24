module WaveFunctionCollapse exposing
    ( Model(..)
    , ModelDetails
    , PropagationTile(..)
    , RandomPick
    , Socket
    , Sockets
    , TerrainType(..)
    , TilesDefinition
    , done
    , init
    , pickTile
    , propagate
    , stopped
    , tileById
    )

import Grid exposing (Grid)
import Random


type Model tileT
    = Model (ModelDetails tileT)


pickTile : Pos -> TileId -> Model tileT -> Model tileT
pickTile pos tileId (Model model) =
    Model
        { model
            | openSteps = PickTile pos tileId :: model.openSteps
        }


stopped : Model tileT -> Bool
stopped (Model { openSteps }) =
    List.isEmpty openSteps


done : Model tileT -> Bool
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


propagate : (RandomPick -> msg) -> Maybe RandomPick -> Model tileT -> ( Model tileT, Cmd msg )
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


type alias TilesDefinition tileT =
    { defaultTile : tileT
    , tileImages : List tileT
    , width : Int
    , height : Int
    , socketsFor : tileT -> Sockets
    }


init : TilesDefinition tileT -> Model tileT
init ({ width, height, tileImages } as tilesDefinition) =
    Model
        { propGrid = Grid.repeat width height (superposition tileImages)
        , openSteps = []
        , tilesDefinition = tilesDefinition
        }


type alias ModelDetails tileT =
    { propGrid : PropagationGrid
    , openSteps : List PropStep
    , tilesDefinition : TilesDefinition tileT
    }


type TerrainType
    = Sand
    | Wall


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


type PropagationTile
    = Fixed TileId
    | Superposition (List TileId)


type alias PropagationGrid =
    Grid PropagationTile


type alias TileId =
    Int


type alias Pos =
    ( Int, Int )


type PropStep
    = PickTile Pos TileId
    | KeepOnlyMatching Pos Pos


superposition : List tileT -> PropagationTile
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


getSocketIn : TilesDefinition tileT -> tileT -> Direction -> Socket
getSocketIn { socketsFor } tileImage dir =
    let
        sockets =
            socketsFor tileImage
    in
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


canDock : ModelDetails tileT -> Direction -> Socket -> Int -> Bool
canDock modelDetails dockDir dockSocket dockTileId =
    let
        dockTile =
            tileById modelDetails dockTileId

        currentSocket =
            getSocketIn modelDetails.tilesDefinition dockTile dockDir
    in
    currentSocket == dockSocket


type alias Candidate =
    { pos : Pos
    , options : List TileId
    }


nextCandidates : ModelDetails tileT -> List Candidate
nextCandidates { propGrid, tilesDefinition } =
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
    Tuple.first <| Grid.foldr f ( [], List.length tilesDefinition.tileImages ) gridWithIdx


pickRandom : RandomPick -> ModelDetails tileT -> ModelDetails tileT
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


processStep : ModelDetails tileT -> PropStep -> PropagationGrid -> ( List PropStep, PropagationGrid )
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
                            getSocketIn modelDetails.tilesDefinition originTile dir

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


randomTileAndTileIdGen : ModelDetails tileT -> Random.Generator ( Int, Int )
randomTileAndTileIdGen { tilesDefinition, propGrid } =
    let
        tileCount =
            Grid.width propGrid * Grid.height propGrid
    in
    Random.pair (Random.int 0 tileCount) (Random.int 0 (List.length tilesDefinition.tileImages))


mkRandom : (RandomPick -> msg) -> ModelDetails tileT -> Cmd msg
mkRandom mkMsg modelDetails =
    Random.generate (\numbers -> mkMsg (RandomPick numbers)) (randomTileAndTileIdGen modelDetails)


tileById : ModelDetails tileT -> Int -> tileT
tileById { tilesDefinition } i =
    case List.head <| List.drop i tilesDefinition.tileImages of
        Nothing ->
            tilesDefinition.defaultTile

        Just aTile ->
            aTile
