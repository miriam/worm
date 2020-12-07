import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Task
import Random

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { command : String,
    x : String,
    y : String,
    wormCoordinates : List (Int, Int),
    time : Time.Posix
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { command = "h",
    x = "300",
    y = "200",
    wormCoordinates = [(200, 50),
    (205, 50),
    (210, 50),
    (215, 50),
    (220, 50),
    (225, 50),
    (230, 50),
    (235, 50),
    (240, 50),
    (245, 50),
    (250, 50)],
    time = (Time.millisToPosix 0)
    }
  , Cmd.batch
      [ Task.perform Tick Time.now ]
  )


-- UPDATE

type Msg
  = Change String
  | Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newCommand ->
      ({ model | command = updateCommand newCommand model.command }
        , Cmd.none )


    Tick newTime ->
      ({ model |
        wormCoordinates = updateWormCoordinates model.wormCoordinates model.command }
        , Cmd.none )

updateWormCoordinates : List(Int,Int) -> String -> List(Int,Int)
updateWormCoordinates coords command =
  let
    head = newSegment (Maybe.withDefault (0,0) (List.head coords)) command
  in
  if List.member head coords || isOutOfBounds head
  then coords
  else head :: (head :: (List.take ((List.length coords)-1) coords))

isOutOfBounds : (Int, Int) -> Bool

isOutOfBounds head =
  Tuple.first head >= 400
    || Tuple.first head <= 100
    || Tuple.second head >= 210
    || Tuple.second head <= 10

newSegment : (Int,Int) -> String -> (Int,Int)
newSegment head command =
  case command of
    "h" ->
      Tuple.mapFirst (\x -> x - 5) head
    "j" ->
      Tuple.mapSecond (\y -> y + 5) head
    "k" ->
      Tuple.mapSecond (\y -> y - 5) head
    "l" ->
      Tuple.mapFirst (\x -> x + 5) head
    _ ->
      Tuple.mapFirst (\x -> x + 5) head

updateCommand : String -> String -> String
updateCommand newCommand oldCommand =
  let
    parsedCommand = String.right 1 newCommand
    commandSequence = String.concat [oldCommand, parsedCommand]
    allowedSequenceVector = "hkljlkhjh"
  in

  if String.contains commandSequence allowedSequenceVector
  then parsedCommand
  else oldCommand

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 250 Tick

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Move with hjkl", value model.command, onInput Change ] []
      , div [] [ Html.text (model.command) ]
      , board model
    ]


board : Model -> Html msg
board model =
  svg
    [ viewBox "0 0 400 400"
    , Svg.Attributes.width "400"
    , Svg.Attributes.height "400"
    ]
    [ rect
        [ x "100"
        , y "10"
        , Svg.Attributes.width model.x
        , Svg.Attributes.height model.y
        , stroke "black"
        , strokeWidth "2"
        ]
        []
      , renderWorm model.wormCoordinates model.command
    ]

renderWorm : List (Int, Int) -> String -> Svg msg
renderWorm coords command =
     polyline
        [ points (coordsToString coords)
        , fill "none"
        , stroke "white"
        , strokeWidth "5"
        ]
        []

coordsToString : List (Int, Int) -> String
coordsToString coords =
  String.join " " (List.map tupleToString coords)

tupleToString : (Int, Int) -> String
tupleToString tuple =
  String.join "," [String.fromInt (Tuple.first tuple)
  , String.fromInt (Tuple.second tuple)]
