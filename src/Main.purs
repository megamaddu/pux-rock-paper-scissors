module Main where

import App.Layout (State, Action(PageView), view, update)
import App.Routes (match)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import Prelude (bind, pure, (=<<), (<<<))
import Pux (App, CoreEffects, Config, Update, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal ((~>))

type AppEffects =
  ( dom :: DOM
  , random :: RANDOM
  )

-- | App configuration
config :: forall e. State -> Eff (dom :: DOM | e) (Config State Action AppEffects)
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> PageView <<< match

  pure
    { initialState: state
    , update: update :: Update State Action AppEffects
    , view
    , inputs: [ routeSignal ]
    }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app