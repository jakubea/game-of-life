module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas
import Color exposing (Color)
import Grid
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Time exposing (Posix)


type alias Model =
    { aliveCellList : List Cell
    , isRunning : Bool
    , time : Float
    }


type alias Cell =
    { x : Int
    , y : Int
    }


type Msg
    = AnimationFrame Posix


init : () -> ( Model, Cmd Msg )
init () =
    ( { aliveCellList =
            [ Cell 0 0
            , Cell 1 1
            , Cell 2 2
            , Cell 3 2
            ]
      , isRunning = True
      , time = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame t ->
            ( { model | time = t |> Time.posixToMillis |> toFloat }, Cmd.none )


height : Float
height =
    500


width : Float
width =
    500


gridSize : Int
gridSize =
    24


cellSize : Float
cellSize =
    (width - padding * 2) / toFloat gridSize


padding : Float
padding =
    width * 0.1


thickness : Float
thickness =
    cellSize * 0.5


view : Model -> Html Msg
view model =
    Canvas.toHtml ( round width, round height )
        []
        (Canvas.shapes [ Canvas.fill Color.white ]
            [ Canvas.rect ( 0, 0 ) width height
            ]
            :: renderItems model
        )


renderItems : Model -> List Canvas.Renderable
renderItems model =
    List.map (\{ x, y } -> renderItem ( x, y )) model.aliveCellList


renderItem : ( Int, Int ) -> Canvas.Renderable
renderItem ( col, row ) =
    let
        ( colf, rowf ) =
            ( toFloat col, toFloat row )

        ( x, y ) =
            ( (rowf * cellSize) + padding + cellSize / 2
            , (colf * cellSize) + padding + cellSize / 2
            )
    in
    Canvas.shapes
        [ Canvas.fill Color.black
        ]
        [ Canvas.circle ( x - thickness, y - thickness ) thickness ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions { isRunning, time } =
    if isRunning then
        onAnimationFrame AnimationFrame

    else
        Sub.none
