module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S

data ClientKind = GovOrgKind | CompanyKind | IndividualKind

data Client i = GovOrg  { clientId :: i , clientName :: String }
              | Company { clientId :: i , clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i , person :: Person }
              deriving Show
 
data Person = Person { firstName :: String, lastName  :: String }
              deriving Show


--classifyClients :: [Client Integer] -> Map ClientKind (Set (Client Integer))
--classifyClients lst = Set.Empty
