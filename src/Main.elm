module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas
import Color exposing (Color)
import Grid
import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra
import SingleArray2D exposing (SingleArray2D)
import Time exposing (Posix)


initialStateOne =
    [ Cell 0 0
    , Cell 0 1
    , Cell 1 1
    , Cell 2 2
    , Cell 3 3
    , Cell 4 2
    , Cell 2 4
    , Cell 4 4
    , Cell 5 4
    , Cell 4 5
    , Cell 5 5
    ]


initialStateTwo =
    [ -- Cell 0 0
      -- ,
      Cell 0 1
    , Cell 1 1
    , Cell 2 2
    , Cell 3 3
    , Cell 4 2
    , Cell 2 4
    , Cell 4 4
    , Cell 5 4
    , Cell 4 5
    , Cell 5 5
    ]


initialStateGlider =
    [ Cell 0 1
    , Cell 1 2
    , Cell 2 0
    , Cell 2 1
    , Cell 2 2
    ]


setup : Setup
setup =
    { tickTime = 100
    , height = 500
    , width = 500
    , gridSize = 25

    -- , initialAliveCellList = initialStateGlider
    -- , initialAliveCellList = initialStateOne
    , initialAliveCellList = initialStateTwo
    }


type alias Setup =
    { tickTime : Miliseconds
    , height : Float
    , width : Float
    , gridSize : Int
    , initialAliveCellList : List Cell
    }


type Model
    = NotReady
    | Ready World


type alias Miliseconds =
    Int


type alias World =
    { cells : SingleArray2D CellState
    , isRunning : Bool
    , lastTime : Miliseconds
    }


type alias Cell =
    { x : Int
    , y : Int
    }


type CellState
    = Alive
    | Dead


type Msg
    = AnimationFrame Posix


type WorldMsg
    = Tick Miliseconds


initWorld : Miliseconds -> World
initWorld time =
    { cells =
        List.foldl (\{ x, y } cells -> SingleArray2D.set ( x, y ) Alive cells)
            (SingleArray2D.repeat setup.gridSize setup.gridSize Dead)
            setup.initialAliveCellList
    , isRunning = True
    , lastTime = time
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( NotReady
    , Cmd.none
    )


getNeighbours : ( Int, Int ) -> SingleArray2D CellState -> List CellState
getNeighbours ( x, y ) cells =
    let
        fullRange =
            [ x - 1, x, x + 1 ]

        zeroRange =
            [ x - 1, x + 1 ]

        fold : Int -> List Int -> List ( Int, Int )
        fold neighbourY cols =
            List.foldl (\neighbourX aggregate -> ( neighbourX, neighbourY ) :: aggregate) [] cols
    in
    [ fold (y - 1) fullRange
    , fold y zeroRange
    , fold (y + 1) fullRange
    ]
        |> List.concat
        |> List.map normalizeCoord
        |> List.map
            (\coord -> SingleArray2D.getWithDefault Dead coord cells)


normalizeCoord : ( Int, Int ) -> ( Int, Int )
normalizeCoord ( x, y ) =
    ( modBy setup.gridSize x
    , modBy setup.gridSize y
    )


updateWorld : WorldMsg -> World -> ( World, Cmd WorldMsg )
updateWorld msg world =
    case msg of
        Tick time ->
            if time - world.lastTime < setup.tickTime then
                ( world, Cmd.none )

            else
                ( { world
                    | cells =
                        world.cells
                            |> SingleArray2D.indexedFoldl
                                (\coord cellState aggregate ->
                                    let
                                        numberOfAlive =
                                            getNeighbours coord world.cells
                                                |> List.Extra.count ((==) Alive)
                                    in
                                    SingleArray2D.set coord
                                        (case numberOfAlive of
                                            2 ->
                                                cellState

                                            3 ->
                                                Alive

                                            _ ->
                                                Dead
                                        )
                                        aggregate
                                )
                                (SingleArray2D.repeat setup.gridSize setup.gridSize Dead)
                    , lastTime = time
                  }
                , Cmd.none
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        NotReady ->
            case msg of
                AnimationFrame t ->
                    let
                        timeMiliseconds =
                            Time.posixToMillis t
                    in
                    ( Ready (initWorld timeMiliseconds), Cmd.none )

        Ready world ->
            case msg of
                AnimationFrame t ->
                    let
                        timeMiliseconds =
                            Time.posixToMillis t
                    in
                    let
                        worldMsg =
                            Tick timeMiliseconds

                        ( newWorld, cmd ) =
                            updateWorld worldMsg world
                    in
                    ( Ready newWorld, Cmd.none )


cellSize : Float
cellSize =
    setup.width / toFloat setup.gridSize


thickness : Float
thickness =
    cellSize * 0.5


view : Model -> Html Msg
view model =
    case model of
        NotReady ->
            Html.div [] []

        Ready world ->
            Canvas.toHtml ( round setup.width, round setup.height )
                [ Html.Attributes.style "width" <| toPx setup.width
                , Html.Attributes.style "height" <| toPx setup.height
                ]
                (Canvas.shapes [ Canvas.fill Color.white ]
                    [ Canvas.rect ( 0, 0 ) setup.width setup.height
                    ]
                    :: renderItems world
                )


renderItems : World -> List Canvas.Renderable
renderItems world =
    SingleArray2D.indexedFoldl
        (\( x, y ) cellState list ->
            case cellState of
                Alive ->
                    renderItem ( x, y ) :: list

                Dead ->
                    list
        )
        []
        world.cells


renderItem : ( Int, Int ) -> Canvas.Renderable
renderItem ( col, row ) =
    let
        ( colf, rowf ) =
            ( toFloat col, toFloat row )

        ( x, y ) =
            ( (rowf * cellSize) + cellSize
            , (colf * cellSize) + cellSize
            )
    in
    Canvas.shapes
        [ Canvas.fill Color.black ]
        [ Canvas.circle ( x - thickness, y - thickness ) thickness ]


toPx : Float -> String
toPx value =
    String.fromFloat value ++ "px"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        isRunning =
            case model of
                NotReady ->
                    True

                Ready world ->
                    world.isRunning
    in
    if isRunning then
        onAnimationFrame AnimationFrame

    else
        Sub.none
