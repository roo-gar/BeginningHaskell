module Chapter2.SimpleFunctions where

{-
This is a multiline comment
-}

-- this is a single line comment

firstOrEmpty :: [[Char]] -> [Char] 
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
(+++) lst1 lst2 = if null lst2 
                  then lst1
                  else (lst1 ++ [head lst2]) +++ (tail lst2) 

myreverse :: [a] -> [a]
myreverse lst = if null lst
                then lst
                else (myreverse (tail lst)) ++ [head lst]

maxmin :: Ord a => [a] -> (a, a)
maxmin lst = let h = head lst
             in if (length lst) == 1
                then (h, h)
                else ( if h > t_max then h else t_max
                     , if h < t_min then h else t_min )
                     where t = maxmin (tail lst)
                           t_max = fst t
                           t_min = snd t

sorted :: [Integer] -> Bool
sorted []            = True
sorted [_]           = True
sorted (x : r@(y:_)) = x < y && sorted r

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n < 0     = Nothing
ifibonacci 0             = Just 0
ifibonacci 1             = Just 1
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))
                           in Just (f1 + f2)

ackerman :: Integer -> Integer -> Integer
ackerman 0 n                     = n + 1
ackerman m 0 | m > 0             = ackerman (m - 1) 1
ackerman m n | m > 0 && n > 0    = ackerman (m - 1) (ackerman m (n - 1))

myunzip :: [(a, a)] -> ([a], [a])
myunzip []    = ([], [])
myunzip (x:xs) = let (a, b) = x
               in (a : first, b : second)
               where unzipped = (myunzip xs)
                     first = (fst unzipped)
                     second = (snd unzipped)