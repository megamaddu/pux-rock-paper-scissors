module App.Game where

import Prelude (const, show, (++), ($), (<$>), (<*>), pure, bind, negate, (+))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (snd, fst, Tuple(Tuple))
import Pux (EffModel, noEffects)
import Pux.Html (Html, text, button, div)
import Pux.Html.Events (onClick)

data GameChoice
  = Rock
  | Paper
  | Scissors
  | Lizard
  | Spock

data GameResult
  = PlayerWins
  | ComputerWins
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
    Just PlayerWins -> state { gameResult = Tuple 1 "Player wins!" }
    Just ComputerWins -> state { gameResult = Tuple (-1) "Computer wins!" }
    Just Tie -> state { gameResult = Tuple 0 "Tie!" }
    _ -> state
  where
    determineWinner player computer =
      if didLeftWin player computer
        then PlayerWins
        else if didLeftWin computer player
          then ComputerWins
          else Tie
    didLeftWin left right =
      case left, right of
        Rock, Scissors -> true
        Rock, Lizard -> true
        Paper, Rock -> true
        Paper, Spock -> true
        Scissors, Paper -> true
        Scissors, Lizard -> true
        Lizard, Spock -> true
        Lizard, Paper -> true
        Spock, Scissors -> true
        Spock, Rock -> true
        _, _ -> false

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
    [ button [ onClick (const $ PlayerMove Rock) ] [ text "Rock" ]
    , button [ onClick (const $ PlayerMove Paper) ] [ text "Paper" ]
    , button [ onClick (const $ PlayerMove Scissors) ] [ text "Scissors" ]
    , button [ onClick (const $ PlayerMove Lizard) ] [ text "Lizard" ]
    , button [ onClick (const $ PlayerMove Spock) ] [ text "Spock" ]
    , div [] [ text $ "Player choice: " ++ printMaybeChoice state.playerChoice ]
    , div [] [ text $ "Computer choice: " ++ printMaybeChoice state.computerChoice ]
    , div [] [ text $ "Game result: " ++ snd state.gameResult ]
    , div [] [ text $ "Score: " ++ show state.score ]
    , button [ onClick (const Reset) ] [ text "Reset!" ]
    ]
