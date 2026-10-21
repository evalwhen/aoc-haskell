module Year2023.Day12 where
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isSpace)
import System.IO (readFile')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Array

solve1 :: IO ()
solve1 = do
    lls <- getRawLines "inputs/2023-12.txt"
    putStrLn $ "length lls: " ++  show (length lls)
    case parseLines lls of
        Nothing -> putStrLn "parseLine failed"
        Just pls -> putStrLn $ "part1: " ++ (show $ countLines pls)


-- #TODO: 我这里想流式读取文件， 就是读一行处理一行怎么读
-- #TODO: 熟悉下 haskell 文件操作的 api, 从底层到高层， 让 ds 写一些 demo 来看下， 可以实际运行
getRawLines :: FilePath -> IO [String]
getRawLines path =  lines <$> readFile' path

parseLines :: [String] -> Maybe [(String, [Int])]
parseLines = traverse parseLine

parseLine :: String -> Maybe (String, [Int])
parseLine = parse
  where
    parse str = 
        case (readP_to_S $ parseLineP <* eof) str of
            [] -> Nothing
            (v :_) -> Just $ fst v


parseLineP :: ReadP (String, [Int])
parseLineP =  (,) <$> left <*> (space *> right)
  where
    left :: ReadP String
    left = many1 $ choice [char '.', char '#', char '?']
    right = sepBy1 number (char ',')
    digit = satisfy isDigit
    number :: ReadP Int
    number = read <$> many1 digit
    space = satisfy isSpace

countLines :: [(String, [Int])] -> Int
countLines ps = sum $ map (uncurry countLine') ps

countLine :: String -> [Int] -> Int
countLine str stack = go str 0 stack
  where
    memoMap :: M.Map (String, Int, [Int]) Int
    memoMap =
      M.fromList
        [ ((s, si, ss), compute s si ss)
        | s <- suffixes str,
          si <- [0 .. maximum stack + 1],
          ss <- suffixes stack
        ]

    go :: String -> Int -> [Int] -> Int
    go str sl stack = fromMaybe (compute str sl stack) $ M.lookup (str, sl, stack) memoMap

    compute :: String -> Int -> [Int] -> Int
    compute "" sl []
       | sl /= 0 = 0
       | otherwise = 1

    compute "" sl (h:t)
          | sl == h && null t = 1
          | otherwise = 0

    compute (c:ct) sl []
          | sl /= 0 = 0
          | c == '#' = 0
          | otherwise = go ct 0 []

    compute str@(c:ct) sl stack@(h:t)
          | sl > h = 0
          | c == '#' = go ct (sl + 1) stack
          | c == '.' && sl == h = go ct 0 t
          | c == '.' && sl == 0 = go ct 0 stack
        --   | c == '?' = go ('#' : ct) sl stack + go ('.' : ct) sl stack
          | c == '?' = go ct (sl + 1) stack
                   + (if sl == h then go ct 0 t else if sl == 0 then go ct 0 stack else 0)
          | otherwise = 0


suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes str@(_:t) = str : suffixes t

-- part2

unfoldInput :: (String, [Int]) -> (String, [Int])
unfoldInput (str, st) = (unfoldStr str, unfoldSt st)
  where
    unfoldStr str = intercalate "?" $ replicate 5 str
    unfoldSt st = concat $ replicate 5 st

solve2 :: IO ()
solve2 = do
    lls <- getRawLines "inputs/2023-12.txt"
    putStrLn $ "length lls: " ++  show (length lls)
    case parseLines lls of
        Nothing -> putStrLn "parseLine failed"
        Just pls -> putStrLn $ "part1: " ++ (show $ countLines $ map unfoldInput pls)


-- dp 版本

-- st means stack
countLine' :: String -> [Int] -> Int
countLine' s st = table ! (0, 0, 0)
  where
    sl    = length s
    stl    = length st
    sta    = listArray (0, stl - 1) st :: Array Int Int
    maxst = maximum st
    sa   = listArray (0, sl - 1) s  :: Array Int Char

    table :: Array (Int, Int, Int) Int
    table = array ((0, 0, 0), (sl, maxst + 1, stl))
              [ ((si, k, sti), go si k sti)
              | si <- [0 .. sl], k <- [0 .. maxst + 1], sti <- [0 .. stl] ]

    -- si: 处理到第几位  k: 当前连续 '#' 长度  sti: 下一组未满足的 group
    go si k sti
      | si == sl    = if sti == stl then fromEnum (k == 0)
                    else fromEnum (sti == stl - 1 && k == sta ! sti)
      | k > sta ! sti = 0
      | c == '#'  = table ! (si + 1, k + 1, sti)
      | c == '.'  = close
      | otherwise = table ! (si + 1, k + 1, sti) + close   -- '?'
      where
        c     = sa ! si
        -- limit = if sti < stl then sta ! sti else 0
        close | k == 0                = table ! (si + 1, 0, sti)
              | sti < stl && k == sta ! sti   = table ! (si + 1, 0, sti + 1)
              | otherwise             = 0