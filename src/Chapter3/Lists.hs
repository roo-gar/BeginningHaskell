module Chapter3.Lists where

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f initial []     = initial
myfoldr f initial (x:xs) = f x (foldr f initial xs)

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl _ initial [] = initial
myfoldl f initial (x:xs) = foldl f (f initial x) xs

data Person = Person { firstName :: String, lastName  :: String }
              deriving Show

myproduct :: [Integer] -> Integer
myproduct [] = 1
myproduct (x:xs) = x * myproduct xs

minimumPerson :: [Person] -> Person
minimumPerson [p] = p
minimumPerson (p1@(Person {firstName = fn1}):p2@(Person {firstName = fn2}):ps) = if length fn1 < length fn2
                                                                                 then minimumPerson $ p1:ps
                                                                                 else minimumPerson $ p2:ps

myall :: [Bool] -> Bool
myall [b] = b
myall (b1:b2:bs) = myall ((b1 && b2) : bs)

myproductf :: [Integer] -> Integer
myproductf = foldr (*) 1 

minimumPersonf :: [Person] -> Person
minimumPersonf = foldr 
                 (\p1@(Person {firstName = fn1}) p2@(Person {firstName = fn2}) -> if length fn1 < length fn2
                                                                                            then p1
                                                                                            else p2) 
                 (Person {firstName = "This is a really long name", lastName = ""})

myallf :: [Bool] -> Bool
myallf = foldr (&&) True

minimumBy :: Ord b => (a -> b) -> [a] -> a
minimumBy g lst@(x:xs) = foldr (\x y -> if (g x) < (g y) then x else y) x lst