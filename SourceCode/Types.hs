module Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (isPrefixOf)
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim s
    | delim `isPrefixOf` s = splitOn delim (drop (length delim) s)
    | s == [] = []
    | otherwise = let (token, rest) = breakPrefix delim s
                  in token : splitOn delim rest
  where
    breakPrefix _ [] = ([], [])
    breakPrefix d s@(x:xs)
        | d `isPrefixOf` s = ([], s)
        | otherwise = let (t, r) = breakPrefix d xs
                      in (x:t, r)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
-- 数据类型
data Symbol = Zero | One | X | Blank deriving (Eq, Show, Read, Ord)  -- 添加 Ord
data Move = LEFT | RIGHT | RESET deriving (Eq, Show, Read)
data State = Q Int | QA | QR deriving (Eq, Ord, Show, Read)
data Variant = CLASSICAL | LRTM | BBTM deriving (Eq, Show, Read)

-- 配置基本数据结构
data TMConfig = TMConfig {
    tape :: [Symbol],
    headPos :: Int,
    currentState :: State,
    rules :: Map (State, Symbol) (State, Symbol, Move),
    variant :: Variant,
    acceptState :: State,
    rejectState :: State
} deriving (Show)

-- 类型转换
charToSymbol :: Char -> Symbol
charToSymbol '0' = Zero
charToSymbol '1' = One
charToSymbol 'X' = X
charToSymbol '*' = Blank
charToSymbol _ = error "Invalid symbol"

symbolToChar :: Symbol -> Char
symbolToChar Zero = '0'
symbolToChar One = '1'
symbolToChar X = 'X'
symbolToChar Blank = '*'

-- 基础纸带操作
updateAt :: Int -> a -> [a] -> [a]
updateAt pos newVal list =
    take pos list ++ [newVal] ++ drop (pos + 1) list

-- 解析描述文件
parseDescFile :: String -> TMConfig
parseDescFile content = 
    let ls = lines content
        initialState = readState $ getValue "initialState" ls 
        acceptStateVal = readState $ getValue "acceptState" ls 
        rejectStateVal = readState $ getValueOrDefault "rejectState" ls "qr"
        variantVal = read $ getValue "variant" ls
        rulesStr = getValue "rules" ls
        rules = parseRules rulesStr
        headPosVal = if variantVal == BBTM then 9 else 0
    in TMConfig {
        tape = [],
        headPos = headPosVal,
        currentState = initialState,
        rules = rules,
        variant = variantVal,
        acceptState = acceptStateVal,
        rejectState = rejectStateVal
    }
    where
        getValue key ls = 
            let prefix = key ++ "="
                matchingLines = filter (isPrefixOf prefix) ls
            in case matchingLines of
                [] -> error $ "No value found for key: " ++ key
                (line:_) -> drop (length prefix) line
        
        getValueOrDefault key ls defaultVal = 
            let prefix = key ++ "="
                matchingLines = filter (isPrefixOf prefix) ls
            in case matchingLines of
                [] -> defaultVal  -- 如果没有找到匹配的行，返回默认值
                (line:_) -> drop (length prefix) line
        parseRules s = Map.fromList $ map parseRule $ splitOn "<>" s
        parseRule ruleStr = 
            let parts = splitOn "," ruleStr
                in if length parts /= 5
                then error $ "Invalid rule format: " ++ ruleStr
                else let [s1, sym1, s2, sym2, m] = parts
                    in ((readState s1, readSymbol sym1), (readState s2, readSymbol sym2, readMove m))
        readState "qa" = QA
        readState "qr" = QR
        readState s = Q (read $ drop 1 s)
        readSymbol "0" = Zero
        readSymbol "1" = One
        readSymbol "X" = X
        readSymbol "*" = Blank
        readMove "LEFT" = LEFT
        readMove "RIGHT" = RIGHT
        readMove "RESET" = RESET
        



--  ghc --make Main.hs -package containers -package process -o coursework  编译语句

--  ./coursework LRTM.desc 111101 M2