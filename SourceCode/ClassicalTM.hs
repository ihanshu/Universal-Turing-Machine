module ClassicalTM where

import Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

executeTM :: TMConfig -> [TMConfig]
executeTM config@TMConfig{ tape = t, headPos = pos, currentState = st, 
                         rules = rs, variant = CLASSICAL, acceptState = acc, rejectState = rej } =
    case st of
        _ | st == acc -> [config]
          | st == rej -> [config]
          | otherwise ->
              let currentSym = getCurrentSymbol pos t
                  transition = Map.lookup (st, currentSym) rs
              in case transition of
                  Nothing -> [config]
                  Just (newState, newSym, move) ->
                      let (newTape, newPos) = applyTransition pos t newSym move
                          newConfig = config { tape = newTape, 
                                             headPos = newPos,
                                             currentState = newState }
                      in config : executeTM newConfig
  where
    getCurrentSymbol pos tape
        | pos < 0 || pos >= length tape = Blank
        | otherwise = tape !! pos

    applyTransition pos tape newSym move = 
        let updatedTape = if pos >= 0 && pos < length tape 
                          then updateAt pos newSym tape 
                          else tape
            (adjustedPos, adjustedTape) = adjustTape pos updatedTape move
        in (adjustedTape, adjustedPos)

    adjustTape pos tape move = case move of
        LEFT  -> if pos == 0 
                 then (0, Blank : tape)
                 else (pos - 1, tape)
        RIGHT -> if pos == length tape - 1 
                 then (pos + 1, tape ++ [Blank])
                 else (pos + 1, tape)
        RESET -> error "RESET not allowed in Classical TM"