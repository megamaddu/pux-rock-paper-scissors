module App.Layout where

import Prelude ((<$>), ($), (#))
import App.Game as Game
import App.Routes (Route(Home, NotFound))
import Control.Monad.Eff.Random (RANDOM)
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.Html (Html, text, a, h1, div)
import Pux.Html.Attributes (href)

data Action
  = Child (Game.Action)
  | PageView Route

type State =
  { route :: Route
  , game :: Game.State
  }

init :: State
init =
  { route: NotFound
  , game: Game.init
  }

update :: forall e. Action -> State -> EffModel State Action (random :: RANDOM | e)
update (PageView route) state = noEffects $ state { route = route }
update (Child action) state =
  Game.update action state.game
    # mapState (state { game = _ })
    # mapEffects Child

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ a [ href "http://www.alexmingoia.com/purescript-pux/" ] [ text "Pux" ]
            , text " Rock, Paper, Scissors!"
            ]
    , case state.route of
        Home -> Child <$> Game.view state.game
        NotFound -> App.NotFound.view state
    ]
