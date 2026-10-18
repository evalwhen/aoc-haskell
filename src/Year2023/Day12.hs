module Year2023.Day12 where
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isSpace)
import System.IO (readFile')

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
countLines ps = sum $ map (uncurry countLine) ps

countLine :: String -> [Int] -> Int
countLine str stack = go str 0 stack
  where
    go :: String -> Int -> [Int] -> Int
    go "" n []
      | n /= 0 = 0
      | otherwise = 1

    go "" n (h:t)
      | n == h && null t = 1
      | otherwise = 0

    go (c:ct) n []
      | n /= 0 = 0
      | c == '#' = 0
      | otherwise = go ct 0 []

    go str@(c:ct) n stack@(h:t)
      | n > h = 0
      | c == '#' = go ct (n + 1) stack
      | c == '.' && n == h = go ct 0 t
      | c == '.' && n /= h && n == 0 = go ct 0 stack
      | c == '?' = (go ('#' : ct) n stack) + (go ('.' : ct) n stack)
      | otherwise = 0