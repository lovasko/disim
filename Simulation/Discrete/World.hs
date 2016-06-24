module Simulation.Discrete.World
( World(..)    -- * -> * -> *
, create       -- Work t s -> t -> s -> Int -> World t s
, run          -- World t s -> HaltCond t s -> [(t, s)]
) where

import Data.Word
import Simulation.Discrete.Calendar
import Simulation.Discrete.Event
import Simulation.Discrete.HaltCond
import System.Random

-- | Simulation world.
data World t s = World (Calendar t s) s Word64 [Int]

-- | Create a new world.
create :: (Num t, Ord t)
       => [(t, Event t s)] -- ^ events to schedule
       -> t                -- ^ initial time
       -> s                -- ^ initial state
       -> Int              -- ^ initial random seed
       -> World t s        -- ^ new world
create events time state seed = World cal state 0 seeds
  where
    cal = foldr (flip schedule) (empty time) events
    seeds = randoms (mkStdGen seed)

-- | Execute closes up-coming events within the world.
step :: (Num t, Ord t)
     => World t s -- ^ world before
     -> World t s -- ^ world after
step (World cal state steps seeds) = World newCal newState (steps+1) newSeeds
  where
    (events, advCal)      = advance cal
    n                     = length events
    newSeeds              = drop n seeds
    advSeeds              = take n seeds
    (newState, newEvents) = foldr stack (state, []) (zip advSeeds events)
    newCal                = foldr (flip schedule) advCal newEvents

-- | Run the world simulation, while checking whether at least one of the
-- halting conditions is being met.
run :: (Num t, Ord t)
    => World t s    -- ^ world
    -> HaltCond t s -- ^ halting condition
    -> [(t, s)]     -- ^ resulting time-state pairs 
run world hc = map result worlds
  where
    worlds    = takeWhile ending (iterate step world)
    ending  w = not (check (info w) hc) && (nEvents w /= 0)
    nEvents w = getCalSize w 
    info    w = (getCalTime w, getState w, getSteps w)
    result  w = (getCalTime w, getState w)

    getState (World _ state _ _) = state
    getSteps (World _ _ steps _) = steps
    getCalTime (World cal _ _ _) = getTime cal
    getCalSize (World cal _ _ _) = getSize cal

