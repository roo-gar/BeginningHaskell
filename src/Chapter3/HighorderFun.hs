module Chapter3.HighorderFun where

equalTuples :: [(Integer,Integer)] -> [Bool]
equalTuples t = map ( \(x,y) -> x == y) t

-- Usage e.g. map (multiplyByN 5) [1,2,3]
multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n*x

filterOnes :: [Integer] -> [Integer]
filterOnes lst = filter (\x -> x == 1) lst 

filterANumber :: [Integer] -> Integer -> [Integer]
filterANumber lst n = filter (\x -> x == n) lst 

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = filter (\x -> not $ f x) lst

data Client i = GovOrg  { clientId :: i , clientName :: String }
              | Company { clientId :: i , clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i , person :: Person }
              deriving Show
 
data Person = Person { firstName :: String, lastName  :: String }
              deriving Show

isGovOrg :: Client i -> Bool
isGovOrg client = case client of
                    GovOrg {} -> True
                    _         -> False

filterGovOrgs :: [Client i] -> [Client i] 
filterGovOrgs clients = filter isGovOrg clients

duplicateOdds :: [Integer] -> [Integer]
duplicateOdds = map (*2) . filter odd


--Functions that take a sequence of arguments are called the curried versions of those that take a tuple
myuncurry :: (a -> b -> c) -> (a,b) -> c
myuncurry f = \(x,y) -> f x y
 
mycurry :: ((a,b) -> c) -> a -> b -> c
mycurry f = \x y -> f (x,y)

--(***) performs the parallel composition of two functions (that is, each of them is applied to a component of a tuple)
(***) :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
f *** g = \(x,y) -> (f x, g y)

duplicate :: a -> (a,a)
duplicate x = (x,x)

-- rewrite the formula 3x + 7(x + 2) using point-free style
formula1 :: Integer -> Integer
formula1 = uncurry (+) . ( ((*7) . (+2)) *** (*3) ) . duplicate

-- flip :: (a -> b -> c) -> (b -> a -> c)