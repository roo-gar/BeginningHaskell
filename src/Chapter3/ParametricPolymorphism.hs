module Chapter3.ParametricPolymorphism where

data Client i = GovOrg  { clientId :: i , clientName :: String }
              | Company { clientId :: i , clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i , person :: Person }
              deriving Show
 
data Person = Person { firstName :: String, lastName  :: String }
              deriving Show

swapTriple (x,y,z) = (y,z,x)

duplicate x = (x,x)

nothing _ = Nothing

index []     = []
index [x]    = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
                                      in  (n+1,x):indexed

maybeA [] = 'a'