{-#OPTIONS_GHC -Wall #-}

-- line number : 2
-- language : haskell top
module TypesAsSets where



-- line number : 386
-- language : haskell top
import Data.Char(ord)



-- line number : 11
-- language : haskell
class (Enum t, Bounded t) => Finite t

listOfAllElements :: Finite t => [t]
listOfAllElements = [ minBound .. maxBound ]

data X = X1 | X2 | X3 deriving (Enum, Bounded, Show)
instance Finite X

data Y = Y1 | Y2      deriving (Enum, Bounded, Show)
instance Finite Y

instance Finite Bool

instance Finite Char

instance Finite ()

instance Finite Int



-- line number : 108
-- language : haskell
-- | declaration of x
x :: Integer
x = 42



-- line number : 114
-- language : haskell
-- | declaration of y
y :: Bool
y = xor True False



-- line number : 121
-- language : haskell
    where xor = (/=)

-- 
-- 
-- -- line number : 137
-- -- language : 
-- -- | function
-- succ :: Integer -> Integer
-- succ x = x + 1
-- 
-- 
-- 
-- -- line number : 144
-- -- language : 
-- -- | another function
-- even :: Integer -> Bool
-- even n = if n `mod` 2 == 0 then True else False
-- 
-- 
-- 
-- -- line number : 163
-- -- language : 
-- -- | type of a pair
-- >>> :t (True, 'c')
-- (True, 'c') :: (Bool, Char)
-- 


-- line number : 171
-- language : haskell
instance (Finite a , Finite b ) => Enum (a,b) where

    toEnum :: (Finite a , Finite b ) => Int -> (a, b)
    toEnum = (!!) $ (,) <$> listOfAllElements <*> listOfAllElements

    fromEnum :: (Finite a , Finite b ) => (a, b) -> Int
    fromEnum ( a , b ) = fromEnum a * length ( listOfAllElements :: [ b ] ) + fromEnum b 

instance ( Finite a , Finite b ) => Finite (a,b)

-- 
-- 
-- -- line number : 185
-- -- language : 
-- -- | elements of a product type
-- >>> listOfAllElements :: [X]
-- [X1,X2,X3]
-- 
-- >>> listOfAllElements :: [Y]
-- [Y1,Y2]
-- 
-- >>> listOfAllElements :: [(X,Y)]
-- [(X1,Y1),(X1,Y2),(X2,Y1),(X2,Y2),(X3,Y1),(X3,Y2)]
-- 
-- >>> listOfAllElements :: [(Char,Bool)]
-- [('\NUL',False),('\NUL',True),('\SOH',False),('\SOH',True), . . . ]
-- 
-- 
-- 
-- -- line number : 203
-- -- language : 
-- -- | first component of a pair
-- fst (a,b) = a
-- 
-- 
-- 
-- -- line number : 208
-- -- language : 
-- -- | second component of a pair
-- snd (a,b) = b
-- 


-- line number : 214
-- language : haskell
-- | function from a product type
xorOnPair :: ( Bool , Bool ) -> Bool
xorOnPair pair = ( fst pair ) /= ( snd pair )



-- line number : 220
-- language : haskell
-- | another function from a product type
xorOnPair' :: ( Bool , Bool ) -> Bool
xorOnPair' ( a , b ) = a /= b

-- 
-- 
-- -- line number : 229
-- -- language : 
-- -- | function to a product type
-- divMod :: Integer -> Integer -> ( Integer , Integer )
-- divMod n m = ( n `div` m , n `mod` m )
-- 
-- 
-- 
-- -- line number : 253
-- -- language : 
-- -- | elements of unit type
-- >>> listOfAllElements :: [()]
-- [()]
-- 


-- line number : 322
-- language : haskell
instance ( Finite a , Finite b ) => Enum (Either a b) where

    toEnum :: ( Finite a , Finite b ) => Int -> Either a b
    toEnum = (!!) $ (++) ( Left <$> listOfAllElements ) ( Right <$> listOfAllElements )

    fromEnum :: ( Finite a , Finite b ) => Either a b -> Int
    fromEnum ( Left a ) = fromEnum a
    fromEnum ( Right b ) = length ( listOfAllElements :: [ a ] ) + fromEnum b

instance ( Finite a , Finite b ) => Bounded (Either a b) where

    minBound :: ( Finite a , Finite b ) => Either a b
    minBound = Left minBound
    
    maxBound :: ( Finite a , Finite b ) => Either a b
    maxBound = Right maxBound

instance ( Finite a , Finite b ) => Finite (Either a b)

-- 
-- 
-- -- line number : 345
-- -- language : 
-- -- | elements of an either type
-- >>> listOfAllElements :: [X]
-- [X1,X2,X3]
-- 
-- >>> listOfAllElements :: [Y]
-- [Y1,Y2]
-- 
-- >>> listOfAllElements :: [Either X Y]
-- [Left X1,Left X2,Left X3,Right Y1,Right Y2]
-- 
-- >>> listOfAllElements :: [Either Bool Char]
-- [Left False,Left True,Right '\NUL',Right '\SOH',Right '\STX', . . . ]
-- 


-- line number : 362
-- language : haskell
-- | function to an either type
feedback :: Integer -> Either Char Integer
--                     Left ~ Char,Integer ~ Right
feedback n
  | n < 3     = Left  'F'
  | otherwise = Right ( 10 * n ) -- multiply by 10 to get percentage



-- line number : 389
-- language : haskell
-- | function from an either type
representAsNumber :: Either Bool Char -> Int
--                   Left ~ Bool,Char ~ Right
representAsNumber ( Left  bool ) = if bool then 1 else 0
representAsNumber ( Right char ) = ord char



-- line number : 409
-- language : haskell
-- | another function from an either type
representAsNumber' :: Either Bool Char -> Int
representAsNumber' ( Left  False ) = 0
representAsNumber' ( Left  True  ) = 1
representAsNumber' ( Right char  ) = ord char

-- 
-- 
-- -- line number : 426
-- -- language : 
-- -- | naive reciprocal
-- reciprocal :: Rational -> Rational
-- reciprocal x = 1/x
-- 
-- 
-- 
-- -- line number : 435
-- -- language : 
-- >>> reciprocal 0
-- *** Exception: Ratio has zero denominator
-- 
-- 
-- 
-- -- line number : 449
-- -- language : 
-- -- | reciprocal using either
-- reciprocal :: Rational -> Either () Rational
-- reciprocal 0 = Left  ()
-- reciprocal x = Right (1/x)
-- 


-- line number : 463
-- language : haskell
-- | function from a maybe type
reciprocal :: Rational -> Maybe Rational
reciprocal 0 = Nothing
reciprocal x = Just (1/x)



-- line number : 479
-- language : haskell
instance Finite t => Enum (Maybe t) where

    toEnum :: Finite t => Int -> Maybe t
    toEnum 0 = Nothing
    toEnum n = Just $ toEnum ( n - 1 )

    fromEnum :: Finite t => Maybe t -> Int
    fromEnum Nothing = 0
    fromEnum (Just t) = 1 + fromEnum t

instance Finite t => Bounded (Maybe t) where

    minBound :: Finite t => Maybe t
    minBound = Nothing

    maxBound :: Finite t => Maybe t
    maxBound = Just maxBound

instance Finite t => Finite ( Maybe t )

-- 
-- 
-- -- line number : 503
-- -- language : 
-- -- | elements of a maybe type
-- >>> listOfAllElements :: [X]
-- [X1,X2,X3]
-- 
-- >>> listOfAllElements :: [Maybe X]
-- [Nothing,Just X1,Just X2,Just X3]
-- 
-- >>> listOfAllElements :: [Y]
-- [Y1,Y2]
-- 
-- >>> listOfAllElements :: [Maybe Y]
-- [Nothing,Just Y1,Just Y2]
-- 
-- >>> listOfAllElements :: [Maybe Bool]
-- [Nothing,Just False,Just True]
-- 
-- >>> listOfAllElements :: [Maybe Char]
-- [Nothing,Just '\NUL',Just '\SOH',Just '\STX',Just '\ETX', . . . ]
-- 


-- line number : 530
-- language : haskell
-- | function to a maybe type
inverseOfReciprocal :: Maybe Rational -> Rational
inverseOfReciprocal Nothing  = 0
inverseOfReciprocal (Just x) = (1/x)

