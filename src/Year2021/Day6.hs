module Year2021.Day6 where

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