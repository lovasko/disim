module Simulation.Discrete.Calendar
( Calendar(..) -- * -> * -> *
, advance      -- Calendar t s -> ([Event t s], Calendar t s)
, empty        -- t -> Calendar t s
, getSize      -- Calendar t s -> n
, getTime      -- Calendar t s -> t
, schedule     -- Calendar t s -> (t, Event t s) -> Calendar t s
) where

import Simulation.Discrete.Event
import qualified Data.Map as Map

data Calendar t s = Calendar (Map.Map t [Event t s]) t

-- | Create an empty calendar.
empty :: (Ord t)
      => t            -- ^ initial time
      -> Calendar t s -- ^ empty calendar
empty = Calendar Map.empty

-- | Return the closest up-coming events and remove the from the calendar.
advance :: (Num t)
        => Calendar t s                -- ^ calendar
        -> ([Event t s], Calendar t s) -- ^ events and new calendar
advance cal@(Calendar m _)
  | Map.null m = ([], cal)
  | otherwise  = (events, Calendar rest time)
  where
    ((time, events), rest) = Map.deleteFindMin m

-- | Schedule new event into the calendar.
schedule :: (Num t, Ord t)
         => Calendar t s   -- ^ old calendar
         -> (t, Event t s) -- ^ new event
         -> Calendar t s   -- ^ new calendar
schedule (Calendar m curT) (schedT, event) = Calendar newM curT
  where
    newM = Map.insertWith (++) (curT + schedT) [event] m

-- | Get the number of scheduled events in the calendar.
getSize :: Calendar t s -- ^ calendar
        -> Int          -- ^ number of scheduled events
getSize (Calendar m _) = Map.size m

-- | Get the current time of the calendar.
getTime :: Calendar t s -- ^ calendar
        -> t            -- ^ current time
getTime (Calendar _ t) = t
