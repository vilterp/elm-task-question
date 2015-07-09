import StartApp
import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Task as T
import Json.Decode as Json exposing ((:=))
import Http
import Debug

main =
  fst viewAndTasks

port tasks : Signal (T.Task String ())
port tasks =
  snd viewAndTasks

externalActions =
  Signal.constant NoOp

viewAndTasks =
  StartApp.start
    { initialState = initialState, view = view, update = update }
    externalActions

initialState = 0

view address model =
  div []
    [ button [ onClick address Decrement ] [ text "--" ]
    , div [] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "++" ]
    ]

type Action = Increment | Decrement | NoOp

update loopback now action model =
  case action of
    Increment ->
      (model + 1, Just (lookupZipCode (toString model)))
    Decrement ->
      (model - 1, Nothing)


lookupZipCode : String -> T.Task String ()
lookupZipCode query =
    let
        url = ("http://api.zippopotam.us/us/" ++ query)
        fixupErrorType = T.mapError (always "ERRR!")
        fetch = fixupErrorType (Http.get places url)
    in
        fetch `T.andThen` always (T.succeed ())


places : Json.Decoder (List String)
places =
  let place =
    Json.object2 (\city state -> city ++ ", " ++ state)
                 ("place name" := Json.string)
                 ("state" := Json.string)
  in
    "places" := Json.list place

