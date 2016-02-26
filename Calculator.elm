module Calculator where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (show)
import StartApp.Simple

-- Model
type Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Power

type alias Operation = (Operator, Float)

type alias Model =
    {accumulator : Float
    , nextOp : Operation
    , prevOp : Operation
    , prevAction : Action
    , decimalPos : Int
    }

init : Model
init = { accumulator = 0
       , nextOp = (Add, 0)
       , prevOp = (Add, 0)
       , prevAction = Clear
       , decimalPos = 0
       }

-- Update
type Action
    = Clear
    | EnterDigit Float
    | EnterOperation Operation
    | Equals
    | DecimalPoint

update : Action -> Model -> Model
update action model =
    case action of
        Clear -> init
        EnterDigit digit ->
            let nextModel = case model.prevAction of
                Equals -> init
                _ -> model
            in { nextModel
               | nextOp = updateOp digit model
               , prevAction = action
               , decimalPos = if model.decimalPos == 0 then 0 else model.decimalPos + 1
               }
        EnterOperation op ->
            case model.prevAction of
                EnterOperation oldOp -> {model | nextOp = op}
                _ -> { accumulator = reduce model
                     , prevOp = model.nextOp
                     , nextOp = op
                     , prevAction = action
                     , decimalPos = 0
                     }
        Equals ->
            let newModel = case model.prevAction of
                Equals -> {model | nextOp = model.prevOp}
                _ -> model
            in
                { accumulator = reduce newModel
                , prevOp = newModel.nextOp
                , nextOp = (Add, 0)
                , prevAction = action
                , decimalPos = 0
                }
        DecimalPoint ->
            if model.decimalPos == 0 then
                {model
                | decimalPos = 1
                , prevAction = action
                }
            else
                {model | prevAction = action}

updateOp digit model =
    if model.decimalPos == 0 then
        (fst model.nextOp, (snd model.nextOp) * 10 + digit)
    else
        (fst model.nextOp, (snd model.nextOp) + digit / 10^(toFloat model.decimalPos))

reduce : Model -> Float
reduce model =
    case model.nextOp of
        (Add, x) -> model.accumulator + x
        (Subtract, x) -> model.accumulator - x
        (Multiply, x) -> model.accumulator * x
        (Divide, x) -> model.accumulator / x
        (Power, x) -> model.accumulator ^ x

mainDisplay : Model -> String
mainDisplay model =
    case model.prevAction of
        EnterDigit x -> toString (snd model.nextOp)
        DecimalPoint -> toString (snd model.nextOp) ++ "."
        _ -> toString model.accumulator

-- View
view : Signal.Address Action -> Model -> Html
view address model =
  div []
   [ text (mainDisplay model)
   , br [] []
   , button [onClick address (EnterDigit 7)] [text "7"]
   , button [onClick address (EnterDigit 8)] [text "8"]
   , button [onClick address (EnterDigit 9)] [text "9"]
   , button [onClick address Clear] [text "C"]
   , br [] []
   , button [onClick address (EnterDigit 4)] [text "4"]
   , button [onClick address (EnterDigit 5)] [text "5"]
   , button [onClick address (EnterDigit 6)] [text "6"]
   , br [] []
   , button [onClick address (EnterDigit 1)] [text "1"]
   , button [onClick address (EnterDigit 2)] [text "2"]
   , button [onClick address (EnterDigit 3)] [text "3"]
   , br [] []
   , button [onClick address (EnterDigit 0)] [text "0"]
   , button [onClick address DecimalPoint] [text "."]
   , button [onClick address Equals] [text "="]
   , br [] []
   , button [onClick address (EnterOperation (Add, 0))] [text "+"] 
   , button [onClick address (EnterOperation (Subtract, 0))] [text "-"] 
   , button [onClick address (EnterOperation (Multiply, 0))] [text "*"] 
   , button [onClick address (EnterOperation (Divide, 0))] [text "/"]
   , button [onClick address (EnterOperation (Power, 0))] [text "^"]
   -- uncomment to display internal state
   -- , br [] []
   -- , text ("Model: " ++ (toString model))
   ]

main =
    StartApp.Simple.start
        {model = init
        , update = update
        , view = view
        }
