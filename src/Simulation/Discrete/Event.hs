module Simulation.Discrete.Event
( Event(..)
, Work
, stack
) where

import System.Random

-- | Simulation event that transforms the state and spawns new events.
newtype Event t s = Event
  { perform :: s             -- ^ state
            -> [Float]       -- ^ random floats between (0.0, 1.0)
            -> (s, Work t s) -- ^ transformed state and spawned events
  }

type Work t s = [(t, Event t s)]

-- | Combinating function for folding multiple events on a state.
stack :: (Int, Event t s)      -- ^ seed and event
      -> (s, [(t, Event t s)]) -- ^ state & spawned events
      -> (s, [(t, Event t s)]) -- ^ state & spawned events after this event
stack (seed, event) (st, evs) = (newSt, newEvs)
  where
    (newSt, evs2) = perform event st (randoms (mkStdGen seed))
    newEvs = evs ++ evs2
