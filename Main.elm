import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Signal exposing (Address, Mailbox, Signal, mailbox)
import Task exposing (Task, andThen, fail, succeed, mapError)
import Json.Decode as Json exposing ((:=))
import Http

----------------------------------------
-- <Basic Counter>
----------------------------------------

type alias Model = Int

type Action = Increment | Decrement

main : Signal Html
main =
  start { model = model, view = view, update = update }

model : Model
model = 0

view : Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]

    , button [ onClick zipCode.address (toString model)]
             [ text "x" ]
    ]

update : UpdateFn
update action model =
  case action of
    Increment -> (model + 1, [1])
    Decrement -> (model - 1, [0])

----------------------------------------
-- </Basic Counter>
----------------------------------------

----------------------------------------
-- <Mailboxes n' Ports>
----------------------------------------
zipCode : Signal.Mailbox String
zipCode =
  mailbox ""

results : Signal.Mailbox (List String)
results =
  mailbox []

type FetchError = EmptyZip
                | HttpError Http.Error
                | NoOp

port fetch : Signal (Task FetchError ())
port fetch =
  let go zip = if zip == ""
               then (fail EmptyZip)
               else (mapError HttpError (lookupZipCode zip)) `andThen`
                    (Signal.send results.address)
  in
    Signal.map go zipCode.signal

type alias BackgroundThingies = Int

backgroundMailbox : Mailbox BackgroundThingies
backgroundMailbox = Signal.mailbox -1

port doBackground : Signal (Task FetchError ())
port doBackground =
    Signal.map
      (\i ->
        if i == 0
          then (lookupZipCode (toString i) |> mapError HttpError) `andThen`
               (Signal.send results.address)
          else fail NoOp
      ) backgroundMailbox.signal

----------------------------------------
-- </Mailboxes n' Ports>
----------------------------------------

----------------------------------------
-- <Tasks>
----------------------------------------
lookupZipCode : String -> Task Http.Error (List String)
lookupZipCode query =
  Http.get places ("http://api.zippopotam.us/us/" ++ query)

places : Json.Decoder (List String)
places =
  let place =
    Json.object2 (\city state -> city ++ ", " ++ state)
                 ("place name" := Json.string)
                 ("state" := Json.string)
  in
    "places" := Json.list place

----------------------------------------
-- </Tasks>
----------------------------------------


----------------------------------------
-- <StartApp>
----------------------------------------

type alias UpdateFn = (Action -> Model -> (Model, List BackgroundThingies))

type alias App model action =
    { model : Model
    , view : Address Action -> Model -> Html
    , update : UpdateFn
    }

actions : Mailbox (Maybe Action)
actions =
  Signal.mailbox Nothing

actionAddress : Address Action
actionAddress =
  Signal.forwardTo actions.address Just

modelStep : UpdateFn -> (Maybe Action) -> Model -> Model
modelStep update (Just action) model =
  update action model |> fst

backgroundActions : UpdateFn -> (Maybe Action) -> Model -> List (Task a ())
backgroundActions update (Just action) model =
  update action model |>
    snd |>
    List.map (\x -> Signal.send backgroundMailbox.address x)

start : App Model Action -> Signal Html
start app =
  let
    model : Signal Model
    model =
      Signal.foldp
        (modelStep app.update)
        app.model
        actions.signal
  in
    Signal.map (app.view actionAddress) model

----------------------------------------
-- </StartApp>
----------------------------------------
