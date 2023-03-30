{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use min" #-}
module Section02 where

firstOrEmpty :: [String] -> String
firstOrEmpty lst =
    if not (null lst)
        then head lst
        else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 =
    if null lst1 {- check emptiness -}
        then lst2 -- base case
        else head lst1 : tail lst1 +++ lst2

-- (+++) Can be written as
(++++) :: [a] -> [a] -> [a]
lst1 ++++ lst2 =
    case lst1 of
        [] -> lst2
        (x : xs) -> x : xs ++++ lst2

-- (+++) Can be written as
(+++++) :: [a] -> [a] -> [a]
[] +++++ lst2 = lst2
(x : xs) +++++ lst2 = x : xs +++++ lst2

rev :: [a] -> [a]
rev lst =
    if null lst
        then lst
        else rev (tail lst) +++ [head lst]

maxmin :: Ord a => [a] -> (a, a)
maxmin lst =
    if null (tail lst) -- one element
        then (head lst, head lst)
        else
            ( if head lst > fst (maxmin (tail lst))
                then head lst
                else fst (maxmin (tail lst))
            , if head lst < snd (maxmin (tail lst))
                then head lst
                else snd (maxmin (tail lst))
            )

maxmin2 :: Ord a => [a] -> (a, a)
maxmin2 lst =
    let
        h = head lst
     in
        if null (tail lst) -- one element
            then (h, h)
            else
                ( if h > t_max then h else t_max
                , if h < t_min then h else t_min
                )
  where
    t = maxmin2 (tail lst)
    t_max = fst t
    t_min = snd t

-- maxmin can also be written as:
maxmin3 :: Ord a => [a] -> (a, a)
maxmin3 [] = error "empty list"
maxmin3 [x] = (x, x)
maxmin3 (x : xs) =
    ( if x > xs_max then x else xs_max
    , if x < xs_min then x else xs_min
    )
  where
    (xs_max, xs_min) = maxmin3 xs

{- This approach is not very type safe -}
clientEx1 :: (String, Integer, [String])
clientEx1 = ("Paul", 25, ["Super Time Machine 2013", "Medieval Machine"])

data Client
    = GovOrg String
    | Company String Integer Person String
    | Individual Person Bool
    deriving (Show)

data Person
    = Person String String Gender
    deriving (Show)

data Gender
    = Male
    | Female
    | Unknown
    deriving (Show)

data Manufacturer
    = IncA
    | IncB
    deriving (Show)

data CanTravel
    = ToThePast
    | ToTheFuture
    | AnyDirection
    deriving (Show)

newtype ModelNumber = ModelNumber Int deriving (Show)

newtype Price = Price Float deriving (Show)

-- TimeMachine IncA (ModelNumber 123) "Super Machine 123" ToThePast (Price 99)
data TimeMachine
    = TimeMachine Manufacturer ModelNumber String CanTravel Price
    deriving (Show)

clientName :: Client -> String
clientName client =
    case client of
        GovOrg name ->
            name
        Company name _id _person _resp ->
            name
        Individual (Person firstName lastName _gender) _isSubscribed ->
            firstName ++ " " ++ lastName

companyName :: Client -> Maybe String
companyName client =
    case client of
        Company name _id _person _resp ->
            Just name
        _ ->
            Nothing

--  EXERCISE 2.5

{-
 Write a function that returns the number of clients of each gender.
 You may need to define an auxiliary data type to hold the results of this function
 -}

newtype MaleCount = MaleCount Int deriving (Show)
newtype FemaleCount = FemaleCount Int deriving (Show)
newtype UnknownCount = UnknownCount Int deriving (Show)

forStats :: [Client] -> (MaleCount, FemaleCount, UnknownCount)
forStats clients =
    let
        toTup m' f' u' = (MaleCount m', FemaleCount f', UnknownCount u')

        newGenderStat m f u g =
            case g of
                Male -> toTup (m + 1) f u
                Female -> toTup m (f + 1) u
                Unknown -> toTup m f (u + 1)

        inner clients' (MaleCount m, FemaleCount f, UnknownCount u) =
            case clients' of
                [] ->
                    toTup m f u
                (GovOrg _ : xs) ->
                    inner xs (newGenderStat m f u Unknown)
                (Company _ _ (Person _ _ g) _ : xs) ->
                    inner xs (newGenderStat m f u g)
                (Individual (Person _ _ g) _ : xs) ->
                    inner xs (newGenderStat m f u g)
     in
        inner
            clients
            (MaleCount 0, FemaleCount 0, UnknownCount 0)

-- forStats clientList
clientList :: [Client]
clientList =
    [ Individual (Person "Benjamin" "Thomas" Male) True
    , GovOrg "NASA"
    , Company "X" 1 (Person "Jane" "Doe" Female) "Manager"
    ]

{-
 Write a function that, given a list of time machines, decreases their price by some percentage.
 -}

applyDiscount :: Float -> [TimeMachine] -> [TimeMachine]
applyDiscount pct machines =
    case machines of
        [] -> []
        (TimeMachine man num name canTravel (Price p) : xs) ->
            TimeMachine man num name canTravel (Price (p * (1 - pct)))
                : applyDiscount pct xs

-- applyDiscount (20/100) catalog
catalog :: [TimeMachine]
catalog =
    [ TimeMachine IncA (ModelNumber 123) "Super Machine 123" ToThePast (Price 100)
    , TimeMachine IncB (ModelNumber 234) "Super Machine 234" ToTheFuture (Price 150)
    ]

isSorted :: [Integer] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : zs) = x < y && isSorted (y : zs)

isSorted' :: [Integer] -> Bool
isSorted' [] = True
isSorted' [_] = True
isSorted' (x : r@(y : _)) = x < y && isSorted r