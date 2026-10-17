module Year2021.Day6 where

import qualified Data.Map.Strict as M

type Fish = Int
type Fishes = [Fish]
type Days = Int

passOneDay :: Fishes -> Fishes
-- #TODO 为什么这里换成 foldMap' 之后如此的慢
passOneDay = foldMap step
  where
    step :: Fish -> [Fish]
    step 0 = [6, 8]
    step f = [f - 1]

run :: Days -> Fishes -> Fishes
run 0 fs = fs 
run n fs = run (n - 1) (passOneDay fs)

solve1 :: Days -> Fishes -> Int
solve1 n fs =  length $ run n fs 

-- part2
aggrByTimer :: [Int] -> [Int]
aggrByTimer fs = map f [0..8]
  where
    f c = length (filter (== c) fs)

passOneDay' :: [Int] -> [Int]
passOneDay' [c0,c1,c2,c3,c4,c5,c6,c7,c8] = [c1,c2,c3,c4,c5,c6,c7+c0,c8,c0] 
passOneDay' _ = error "期望九个 count"

run' 0 cs = cs
run' n cs = run' (n - 1) (passOneDay' cs)

solve2 :: Int -> [Int] -> Int
solve2 n fs = sum $ run' n (aggrByTimer fs)

-- use Map

type Fishes' = M.Map Int Int

-- Map 自带的 Monoid 实例是左偏的 union，撞 key 时会静默吞掉一份计数。
-- 包一层，让 <> 变成 unionWith (+)，这样 M.foldMapWithKey 才是按计数相加。
newtype SumMap = SumMap { unSumMap :: Fishes' }
  deriving (Show, Eq)

instance Semigroup SumMap where
  SumMap a <> SumMap b = SumMap (M.unionWith (+) a b)

instance Monoid SumMap where
  mempty = SumMap M.empty

passOneDay'' :: Fishes' -> Fishes'
passOneDay'' = unSumMap . M.foldMapWithKey step
  where
    step :: Int -> Int -> SumMap
    step 0 n = SumMap $ M.fromList [(6, n), (8, n)]
    step i n = SumMap $ M.singleton (i - 1) n

run'' :: Int -> Fishes' -> Fishes'
run'' 0 fs = fs
run'' n fs = run'' (n - 1) (passOneDay'' fs)

solve2' :: Int -> [Int] -> Int
solve2' n fs = sum $ M.elems (run'' n start)
  where
    start = M.fromListWith (+) $ zip fs (repeat 1)

-- 同一个算法，改用 unionsWith 直接指定合并函数，不需要自定义 Monoid

passOneDay''' :: Fishes' -> Fishes'
passOneDay''' fs = M.unionsWith (+) $ map (uncurry step) (M.toList fs)
  where
    step :: Int -> Int -> Fishes'
    step 0 n = M.fromList [(6, n), (8, n)]
    step i n = M.singleton (i - 1) n

run''' :: Int -> Fishes' -> Fishes'
run''' 0 fs = fs
run''' n fs = run''' (n - 1) (passOneDay''' fs)

solve2'' :: Int -> [Int] -> Int
solve2'' n fs = sum $ M.elems (run''' n start)
  where
    start = M.fromListWith (+) $ zip fs (repeat 1)
