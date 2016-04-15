module Main where

import App.Layout (Action(PageView), State, view, update)
import App.Routes (match)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import Debug.Trace (traceAny)
import Prelude (bind, return)
import Pux (App, CoreEffects, renderToDOM, start)
import Pux.Router (sampleUrl)
import Signal ((~>))

type AppEffects =
  ( dom :: DOM
  , random :: RANDOM
  )

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  app <- start
    { initialState: state
    , update:
        -- | Logs all actions and states (removed in production builds).
        (\a s -> traceAny {action: a, state: s} (\_ -> update a s))
    , view: view
    , inputs: [routeSignal] }

  renderToDOM "#app" app.html

  -- | Used by hot-reloading code in support/index.js
  return app
