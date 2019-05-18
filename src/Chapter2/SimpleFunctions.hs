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