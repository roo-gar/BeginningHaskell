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