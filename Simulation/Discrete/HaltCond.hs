module Simulation.Discrete.HaltCond
( HaltCond(..)
, check
) where

import Data.Word

-- | All possible halting conditions for the world simulation.
data HaltCond t s
  = HaltSteps Word64       -- ^ after number of steps
  | HaltTime t             -- ^ after reaching certain time
  | HaltState (s -> Bool)  -- ^ state satisfies predicate
  | HaltAll [HaltCond t s] -- ^ multiple base conditions
  | HaltAny [HaltCond t s] -- ^ at least one base condition
  | HaltNone               -- ^ never-ending simulation

-- | Check whether a halting condition is met.
check :: (Num t, Ord t)
      => (t, s, Word64) -- ^ world
      -> HaltCond t s   -- ^ halting condition
      -> Bool           -- ^ halting decision
check (time, _, _) (HaltTime  timeLimit)   = time >= timeLimit
check (_, state, _) (HaltState predicate)  = predicate state
check (_, _, steps) (HaltSteps stepsLimit) = steps >= stepsLimit
check world (HaltAll hcs)                  = all (check world) hcs
check world (HaltAny hcs)                  = any (check world) hcs
check _ HaltNone                           = False

