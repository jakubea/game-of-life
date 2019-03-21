module SingleArray2D exposing
    ( SingleArray2D
    , get
    , getWithDefault
    , indexedFoldl
    , repeat
    , set
    )

import Array exposing (Array)


type SingleArray2D t
    = SingleArray2DInstance Int (Array t)



-- Basic Functions --


indexedFoldl : (( Int, Int ) -> a -> b -> b) -> b -> SingleArray2D a -> b
indexedFoldl func baseCase (SingleArray2DInstance numColumns array1d) =
    let
        func1d item ( index, aggregate ) =
            ( index + 1
            , func (indexToColumnRow numColumns index) item aggregate
            )

        ( _, finalAggregate ) =
            Array.foldl func1d ( 0, baseCase ) array1d
    in
    finalAggregate


repeat : Int -> Int -> a -> SingleArray2D a
repeat numColumns numRows e =
    let
        columns =
            max 1 numColumns

        rows =
            max 1 numRows

        array =
            Array.repeat (columns * rows) e
    in
    SingleArray2DInstance numColumns array


set : ( Int, Int ) -> a -> SingleArray2D a -> SingleArray2D a
set coord newValue (SingleArray2DInstance numColumns array1d) =
    let
        newArray1d =
            Array.set (columnRowToIndex numColumns coord) newValue array1d
    in
    SingleArray2DInstance numColumns newArray1d


get : ( Int, Int ) -> SingleArray2D a -> Maybe a
get coord (SingleArray2DInstance numColumns array1d) =
    Array.get (columnRowToIndex numColumns coord) array1d



-- Higher Functions --


getWithDefault : a -> ( Int, Int ) -> SingleArray2D a -> a
getWithDefault default coord array2d =
    get coord array2d
        |> Maybe.withDefault default



-- Util --


indexToColumnRow : Int -> Int -> ( Int, Int )
indexToColumnRow numColumns index =
    let
        column =
            remainderBy numColumns index

        row =
            index // numColumns
    in
    ( column, row )


columnRowToIndex : Int -> ( Int, Int ) -> Int
columnRowToIndex numColumns ( column, row ) =
    column + numColumns * row
