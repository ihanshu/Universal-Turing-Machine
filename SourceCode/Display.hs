module Display where

import Types
import qualified ClassicalTM as CTM
import qualified LRTM as LTM
import qualified BBTM as BTM
import System.Environment (getArgs)
import System.IO (readFile)
import Control.Concurrent (threadDelay)
import System.Process
import Data.List (intercalate)
import Data.Map (toList)  -- 导入 toList 函数
import System.Info (os)
import Control.Exception (try, SomeException)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [descFile, inputStr, "M1"] -> runM1 descFile inputStr
        [descFile, inputStr, "M2"] -> runM2 descFile inputStr
        [descFile, "M1"] -> runM1 descFile "00000000000000000000"
        [descFile, "M2"] -> runM2 descFile "00000000000000000000"
        _ -> putStrLn "Usage: coursework <descFile> <input> <M1|M2>"

runM1 :: FilePath -> String -> IO ()
runM1 descFile inputStr = do
    content <- readFile descFile
    let config = parseDescFile content
        inputSymbols = map charToSymbol inputStr
        initialConfig = config { tape = inputSymbols ++ replicate (20 - length inputSymbols) Blank }
        executionTrace = case variant config of
            CLASSICAL -> CTM.executeTM initialConfig
            LRTM      -> LTM.executeLRTM initialConfig
            BBTM      -> BTM.executeBBTM initialConfig
        finalState = currentState $ last executionTrace
    putStrLn $ if finalState == acceptState config then "ACCEPTED" else "REJECTED"

runM2 :: FilePath -> String -> IO ()
runM2 descFile inputStr = do
    content <- readFile descFile
    let config = parseDescFile content
        inputSymbols = map charToSymbol inputStr
        initialConfig = config { tape = inputSymbols ++ replicate (20 - length inputSymbols) Blank }
        executionTrace = case variant config of
            CLASSICAL -> CTM.executeTM initialConfig
            LRTM      -> LTM.executeLRTM initialConfig
            BBTM      -> BTM.executeBBTM initialConfig
    safeClearScreen
    mapM_ (displayStep config) executionTrace
    putStrLn $ "\nFinal state: " ++ show (currentState $ last executionTrace)

displayStep :: TMConfig -> TMConfig -> IO ()
displayStep originalConfig config = do
    safeClearScreen
    putStrLn "Universal Turing Machine - Execution Visualization"
    putStrLn $ "Variant: " ++ show (variant originalConfig)
    putStrLn $ "Current State: " ++ show (currentState config)
    putStrLn "\nTape:"
    putStrLn $ tapeString config
    putStrLn $ headPointer config
    putStrLn "\nRules:"
    putStrLn $ showRules originalConfig
    threadDelay 3000000 -- 3秒延迟

-- 安全的清屏函数，处理可能的异常
safeClearScreen :: IO ()
safeClearScreen = do
    result <- try clearScreen :: IO (Either SomeException ())
    case result of
        Left _ -> do
            -- 如果清屏失败，输出足够的换行符来模拟清屏
            putStrLn (replicate 50 '\n')
        Right _ -> return ()

-- 根据操作系统选择清屏命令
clearScreen :: IO ()
clearScreen = 
    if os == "mingw32" || os == "win32"
    then callCommand "cls"
    else callCommand "clear"

tapeString :: TMConfig -> String
tapeString config = intercalate " | " $ map (show . symbolToChar) (tape config)

headPointer :: TMConfig -> String
headPointer config =
    replicate (6 * headPos config + 1) ' ' ++ "^"

showRules :: TMConfig -> String
showRules config = 
    let ruleList = toList (rules config)  -- 使用 toList
        formatRule ((s1, sym1), (s2, sym2, move)) = 
            show s1 ++ ", " ++ show sym1 ++ " -> " ++ show s2 ++ ", " ++ show sym2 ++ ", " ++ show move
    in intercalate "\n" $ map formatRule ruleList