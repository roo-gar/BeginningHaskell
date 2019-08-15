module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving Show

data Client i = GovOrg  { clientId :: i , clientName :: String }
              | Company { clientId :: i , clientName :: String , person :: Person , duty :: String }
              | Individual { clientId :: i , person :: Person }
              deriving Show
 
data Person = Person { firstName :: String, lastName  :: String }
              deriving Show

{-
classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients [] = M.empty
classifyClients (x : xs) = M.unionWith
                           S.union
	                       (case x of
                            GovOrg _ _-> M.singleton GovOrgKind (S.singleton x)
                            Company _ _ _ _ -> M.singleton CompanyKind (S.singleton x)
                            Individual _ _ -> M.singleton IndividualKind (S.singleton x))
	                       $ classifyClients xs
	         -}              