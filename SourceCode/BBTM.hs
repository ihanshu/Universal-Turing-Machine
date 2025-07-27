module BBTM where

import Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

executeBBTM :: TMConfig -> [TMConfig]
executeBBTM config@TMConfig{ tape = t, headPos = pos, currentState = st, 
                           rules = rs, variant = BBTM, acceptState = acc, rejectState = rej } =
    case st of
        _ | st == acc -> [config]
          | st == rej -> [config]
          | otherwise ->
              let currentSym = t !! pos
                  transition = Map.lookup (st, currentSym) rs
              in case transition of
                  Nothing -> [config]
                  Just (newState, newSym, move) ->
                      let newTape = updateAt pos newSym t
                          newPos = case move of
                                    LEFT  -> max 0 (pos - 1)
                                    RIGHT -> min 19 (pos + 1)
                                    RESET -> 9
                          newConfig = config { tape = newTape, 
                                              headPos = newPos,
                                              currentState = newState }
                      in config : executeBBTM newConfig
executeBBTM _ = error "This function only works for BBTM variant"

initializeBBTM :: Map (State, Symbol) (State, Symbol, Move) -> State -> State -> TMConfig
initializeBBTM rules accept reject =
    TMConfig
        { tape = replicate 20 Zero
        , headPos = 9
        , currentState = Q 0
        , rules = rules
        , variant = BBTM
        , acceptState = accept
        , rejectState = reject
        }