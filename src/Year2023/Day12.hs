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
        [ ((s, i, ss), compute s i ss)
        | s <- suffixes str,
          i <- [0 .. maximum stack + 1],
          ss <- suffixes stack
        ]

    go :: String -> Int -> [Int] -> Int
    go str n stack = fromMaybe (compute str n stack) $ M.lookup (str, n, stack) memoMap

    compute :: String -> Int -> [Int] -> Int
    compute "" n []
       | n /= 0 = 0
       | otherwise = 1

    compute "" n (h:t)
          | n == h && null t = 1
          | otherwise = 0

    compute (c:ct) n []
          | n /= 0 = 0
          | c == '#' = 0
          | otherwise = go ct 0 []

    compute str@(c:ct) n stack@(h:t)
          | n > h = 0
          | c == '#' = go ct (n + 1) stack
          | c == '.' && n == h = go ct 0 t
          | c == '.' && n == 0 = go ct 0 stack
        --   | c == '?' = go ('#' : ct) n stack + go ('.' : ct) n stack
          | c == '?' = go ct (n + 1) stack
                   + (if n == h then go ct 0 t else if n == 0 then go ct 0 stack else 0)
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


countLine' :: String -> [Int] -> Int
countLine' s gs = table ! (0, 0, 0)
  where
    n    = length s
    m    = length gs
    g    = listArray (0, m - 1) gs :: Array Int Int
    maxg = maximum gs
    sv   = listArray (0, n - 1) s  :: Array Int Char

    table :: Array (Int, Int, Int) Int
    table = array ((0, 0, 0), (n, maxg + 1, m))
              [ ((i, k, j), go i k j)
              | i <- [0 .. n], k <- [0 .. maxg + 1], j <- [0 .. m] ]

    -- i: 处理到第几位  k: 当前连续 '#' 长度  j: 下一组未满足的 group
    go i k j
      | i == n    = if j == m then fromEnum (k == 0)
                    else fromEnum (j == m - 1 && k == g ! j)
      | k > limit = 0
      | c == '#'  = table ! (i + 1, k + 1, j)
      | c == '.'  = close
      | otherwise = table ! (i + 1, k + 1, j) + close   -- '?'
      where
        c     = sv ! i
        limit = if j < m then g ! j else 0
        close | k == 0                = table ! (i + 1, 0, j)
              | j < m && k == g ! j   = table ! (i + 1, 0, j + 1)
              | otherwise             = 0