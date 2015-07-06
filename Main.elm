import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Signal exposing (Address, Mailbox, Signal, mailbox)
import Task exposing (Task, andThen)
import Json.Decode as Json exposing ((:=))
import Http
import Maybe.Extra exposing (isJust)

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

update : Action -> Model -> Model
update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1

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

port fetch : Signal (Task Http.Error ())
port fetch =
  let return zip = lookupZipCode zip `andThen`
                   (Signal.send results.address)
  in
    Signal.map return zipCode.signal

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
type alias UpdateFn = (Action -> Model -> Model)

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
  update action model

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
