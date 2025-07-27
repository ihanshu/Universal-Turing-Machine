module LRTM where

import Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

executeLRTM :: TMConfig -> [TMConfig]
executeLRTM config@TMConfig{ tape = t, headPos = pos, currentState = st, 
                           rules = rs, variant = LRTM, acceptState = acc, rejectState = rej } =
    case st of
        _ | st == acc -> [config]
          | st == rej -> [config]
          | otherwise ->
              let currentSym = if pos < length t && pos >= 0 then t !! pos else Blank
                  transition = Map.lookup (st, currentSym) rs
              in case transition of
                  Nothing -> [config]
                  Just (newState, newSym, move) ->
                      let newTape = if pos < length t && pos >= 0 
                                    then updateAt pos newSym t 
                                    else t
                          newPos = case move of
                                    LEFT  -> error "LEFT not allowed in LRTM"
                                    RIGHT -> pos + 1
                                    RESET -> 0
                          newConfig = config { tape = newTape, 
                                              headPos = newPos,
                                              currentState = newState }
                      in config : executeLRTM newConfig
executeLRTM _ = error "This function only works for LRTM variant"