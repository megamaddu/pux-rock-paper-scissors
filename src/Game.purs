module App.Game where

import Prelude (const, show, (<>), ($), (<$>), (<*>), pure, bind, negate, (+))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (snd, fst, Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.String (toLower)
import Pux (EffModel, noEffects)
import Pux.Html (Html, text, button, div, hr)
import Pux.Html.Attributes (className)
import Pux.Html.Events (onClick)

data GameChoice
  = Rock
  | Paper
  | Scissors
  | Lizard
  | Spock

data GameResult
  = PlayerWins String
  | ComputerWins String
  | Tie

data Action
  = PlayerMove GameChoice
  | ComputerMove GameChoice
  | UpdateScore
  | Reset

type State =
  { playerChoice :: (Maybe GameChoice)
  , computerChoice :: (Maybe GameChoice)
  , gameResult :: Tuple Int String
  , score :: Int
  }

init :: State
init =
  { playerChoice: Nothing
  , computerChoice: Nothing
  , gameResult: Tuple 0 "choose..!"
  , score: 0
  }

update :: forall e. Action -> State -> EffModel State Action (random :: RANDOM | e)
update Reset state = noEffects init
update UpdateScore state = noEffects $ updateScore state
update (PlayerMove choice) state =
  { state: state { playerChoice = Just choice }
  , effects: [ do
      computerChoice <- liftEff getRandomChoice
      pure $ ComputerMove computerChoice
    ]
  }
update (ComputerMove choice) state =
  { state: state { computerChoice = Just choice }
  , effects: [ pure $ UpdateScore ]
  }

updateScore :: State -> State
updateScore state =
  let newState = setGameResult state
  in newState { score = newState.score + fst newState.gameResult }

setGameResult :: State -> State
setGameResult state =
  case determineWinner <$> state.playerChoice <*> state.computerChoice of
    Just (PlayerWins reason) -> state { gameResult = 1 /\ ("Player wins!  " <> reason <> "!") }
    Just (ComputerWins reason) -> state { gameResult = -1 /\ ("Computer wins!  " <> reason <> "!") }
    Just Tie -> state { gameResult = 0 /\ "It's a tie!" }
    _ -> state
  where
    determineWinner player computer =
      let
        didPlayerWin = didLeftWin player computer
        didComputerWin = didLeftWin computer player
      in 
        if fst didPlayerWin
          then PlayerWins (snd didPlayerWin)
          else if fst (didLeftWin computer player)
            then ComputerWins (snd didComputerWin)
            else Tie

    didLeftWin left right =
      case left, right of
        Rock, Scissors -> true /\ "Rock crushes Scissors"
        Rock, Lizard -> true /\ "Rock smashes Lizard"
        Paper, Rock -> true /\ "Paper covers Rock"
        Paper, Spock -> true /\ "Paper disproves Spock"
        Scissors, Paper -> true /\ "Scissors cuts Paper"
        Scissors, Lizard -> true /\ "Scissors decapitates Lizard"
        Lizard, Paper -> true /\ "Lizard eats Paper"
        Lizard, Spock -> true /\ "Lizard poisons Spock"
        Spock, Rock -> true /\ "Spock vaporizes Rock"
        Spock, Scissors -> true /\ "Spock disassembles Scissors"
        _, _ -> false /\ "loss"

getRandomChoice :: forall e. Eff (random :: RANDOM | e) GameChoice
getRandomChoice = do
  i <- randomInt 1 5
  pure case i of
    1 -> Rock
    2 -> Paper
    3 -> Scissors
    4 -> Lizard
    _ -> Spock

printMaybeChoice :: Maybe GameChoice -> String
printMaybeChoice Nothing = "-- choosing --"
printMaybeChoice (Just choice) = printChoice choice
  where
    printChoice Rock = "Rock"
    printChoice Paper = "Paper"
    printChoice Scissors = "Scissors"
    printChoice Lizard = "Lizard"
    printChoice Spock = "Spock"

view :: State -> Html Action
view state =
  div
    []
    [ choiceButton Rock "rock" "Rock"
    , choiceButton Paper "paper" "Paper"
    , choiceButton Scissors "scissors" "Scissors"
    , choiceButton Lizard "lizard" "Lizard"
    , choiceButton Spock "spock" "Spock"
    , divider
    , showChoice state.playerChoice
    , showChoice state.computerChoice
    , showGameResult state.gameResult
    , showPlayerScore state.score
    , divider
    , resetButton
    ]
  where
    choiceButton gameChoice choiceClass choiceText =
      button
        [ onClick (const $ PlayerMove gameChoice), className ("choice " <> choiceClass) ]
        [ text choiceText ]

    resetButton = button [ onClick (const Reset), className "reset" ] [ text "Reset!" ]

    showChoice gameChoice = div [ className ("card " <> toLower (printMaybeChoice gameChoice)) ] []

    showGameResult gameResult = div [] [ text $ "Game result: " <> snd gameResult ]
    
    showPlayerScore playerScore = div [] [ text $ "Score: " <> show playerScore ]

    divider = hr [] []