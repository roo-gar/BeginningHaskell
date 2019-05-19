{-# LANGUAGE ViewPatterns #-}
module DataTypes where

data Gender = Male | Female | Unknown
            deriving Show

data Person = Person String String Gender
            deriving Show

data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person
            deriving Show

{-
e.g. Gender
> Male
Male
> :t Female
Female :: Gender
e.g. Person
> Person "Joe" "Smith" Male
Person "Joe" "Smith" Male
> :t Person "Maria" "Kerry" Female
Person "Maria" "Kerry" Female :: Person
e.g.  Client
> Individual (Person "Joe" "Smith" Male)
Individual (Person "Joe" "Smith" Male)
> :t Individual (Person "Javier" "Rodriguez" Unknown)
Individual (Person "Javier" "Rodriguez" Unknown) :: Client
> :t Company "FB" 1992 (Person "Mark" "Zuckerberg" Unknown) "Miami"
Company "FB" 1992 (Person "Mark" "Zuckerberg" Unknown) "Miami" :: Client
-}

clientName :: Client -> String
clientName client = case client of
                      GovOrg  name       -> name
                      Company name _ _ _ -> name
                      Individual person  ->
                          case person of Person fName lName _ -> fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
                       _                  -> Nothing

countGenders :: [Client] -> (Int, Int)
countGenders listClients = if null listClients
                           then (0, 0)
                           else case (head listClients) of
                                    Individual person -> case person of
                                        Person _ _ gender -> case gender of
                                            Male    -> ((fst count) + 1, snd count)
                                            Female  -> (fst count, (snd count) + 1)
                                            Unknown -> count
                                    _                 -> count
                                where count = countGenders (tail listClients)

specialClient :: Client -> Bool
specialClient ( clientName -> "Mr. Alejandro") = True
specialClient _                                = False

--                             Manufacturer Model Name CanTravelToPast CanTravelToFuture Price
data TimeMachine = TimeMachine Manufacturer Int String Bool Bool Float 
                 deriving Show
--                               FirstName LastName
data Manufacturer = Manufacturer String String
                  deriving Show

getDiscounts :: [TimeMachine] -> Float -> [TimeMachine]
getDiscounts listTimeMachines discount = if null listTimeMachines
                                         then []
                                         else case (head listTimeMachines) of
                                                 TimeMachine manufacturer model name canTravelToPast canTravelToFuture price ->
                                                     TimeMachine manufacturer model name canTravelToPast canTravelToFuture (price * (1 - discount / 100)) : (getDiscounts (tail listTimeMachines) discount)

