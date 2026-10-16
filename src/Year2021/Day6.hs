module Year2021.Day6 where

type Fish = Int
type Fishes = [Fish]
type Days = Int

passOneDay :: Fishes -> Fishes
passOneDay = foldMap step
  where
    step :: Fish -> [Fish]
    step 0 = [6, 8]
    step f = [f - 1]

run :: Days -> Fishes -> Fishes
run 0 fs = fs 
run n fs = run (n - 1) (passOneDay fs)

-- #TODO 性能优化， 这里 length 可能有点慢， 改成迭代递归直接求和或者改成 foldl', 要看下可读性第一
solve1 :: Days -> Fishes -> Int
solve1 n fs =  length $ run n fs 