-- Problems 1 to 10 https://wiki.haskell.org/99_questions/1_to_10

-- 01
myLast :: [a] -> a
myLast [] = error "empty lists!"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- 02
myButLast :: [a] -> a
myButLast [] = error "empty lists!"
myButLast [x] = error "only 1 element!"
myButLast (x : [_]) = x
myButLast (_ : xs) = myButLast xs

-- 03
elementAt :: [a] -> Int -> a
elementAt [] _ = error "out of bounds!"
elementAt (x : xs) id
  | id < 1 = error "out of bounds!"
  | id == 1 = x
  | otherwise = elementAt xs (id -1)

-- 04
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- 05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- 06
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = myReverse l == l

-- 07
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten ns = case ns of
  Elem x -> [x]
  List [] -> []
  List (x : xs) -> flatten x ++ flatten (List xs)

-- 08
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (f x xs)
  where
    f _ [] = []
    f x (y : ys)
      | x == y = f x ys
      | otherwise = y : ys

-- 09
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x : xs) = replicate (n + 1) x : pack (drop n xs)
  where
    f _ [] = 0
    f x (y : ys)
      | x == y = 1 + f x ys
      | otherwise = 0
    n = f x xs

-- 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x : xs) = (n + 1, x) : encode (drop n xs)
  where
    f _ [] = 0
    f x (y : ys)
      | x == y = 1 + f x ys
      | otherwise = 0
    n = f x xs
