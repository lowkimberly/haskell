
-- 1
prefix:: Eq a => [a] -> [a] -> Bool
prefix [] [] = True
prefix [] (y:ys) = True
prefix (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = False

-- 2
delete :: Int -> [a] -> [a]
delete k (x:xs) = delete2 k (x:xs) k

--m is the original k, not decremented
delete2 :: Int -> [a] -> Int -> [a]
delete2 k [] m = []
delete2 k (x:xs) m
	| k == 1 = delete2 m (xs) m -- if k = 1, delete the beginning of the list, BUT start again using the tail
	| otherwise = x: (delete2 (k-1) xs m) --otherwise, save the head, and decrement k

--3
data Formula = Atom Int | And Formula Formula | Or Formula Formula | Not Formula

collect_atoms::Formula -> [Int]
collect_atoms (Atom i) = [i]
collect_atoms (And f g) = collect_atoms(f) ++ collect_atoms(g)
collect_atoms (Or f g) = collect_atoms(f) ++ collect_atoms(g)
collect_atoms (Not f) = collect_atoms(f)
	
--4
isort::[Int] -> [Int]
isort (x:xs) = isort_help [x] xs

isort_help::[Int] -> [Int] -> [Int] --sorted and unsorted
isort_help (x:xs) [] = (x:xs) --if the unsorted is empty, return sorted
isort_help [] (y:ys) = [y] -- if sorted list is empty, y must be greater than everything
isort_help (x:xs) (y:ys)
	| y > x   =  isort_help ( x: isort_help xs [y]) ys -- y greater than first in sorted. sort y in tail of sorted.
	| y <= x   = isort_help (y:x:xs) ys --unsorted int is less than x, and therefore less than xs.

--5
rotate :: Int -> [a] -> [a]
rotate n (x:xs)
	| n == 0 = x:xs 
	| otherwise =rotate (n-1) ([last(xs)] ++ [x] ++ init(xs))
	