{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}

module Change where
import Data.Functor ((<&>))
import Typst

data Change a = UnChanged a | Changed a a a a deriving (Functor,Show)

after :: Change a -> a
after ( UnChanged a ) = a
after ( Changed _ _ y _ ) = y

class ShowChange a where
    customizationOfBefore :: a -> a
    customizationOfBefore = id
    customizationOfAfter :: a -> a
    customizationOfAfter = id

showChange :: ShowChange a => a -> a -> Change a
showChange x y = Changed x ( customizationOfBefore x ) y ( customizationOfAfter y )

instance Applicative Change where

    pure :: a -> Change a
    pure = UnChanged

    (<*>) :: Change (a -> b) -> Change a -> Change b
    (UnChanged f) <*> x = f <$> x
    f <*> (UnChanged x) = f <&>($ x)
    (Changed f1 f2 f3 f4) <*> (Changed x1 x2 x3 x4) = Changed (f1 x1) (f2 x2) (f3 x3) (f4 x4)

instance Content a => Content (Change a) where
    toContent :: Content a => Change a -> ContentWrapper
    toContent (UnChanged u    ) = Wrap ( "$ #[ "++unWrap(toContent u)++" ] --> \"NO CHANGE\" $" )
    toContent (Changed _ b _ a) = Wrap ( "$ #[ "++unWrap(toContent b)++" ] --> #[ "++unWrap(toContent a)++" ] $" )

-- | given a a function and a list, \ 
-- change only the first element of the list that the function effects an actual change on
applyFirstChange :: [a -> Change a] -> [a] -> Change [a]

applyFirstChange _ [] = UnChanged []

applyFirstChange [] _ = error "less changes than befores in applyFirstChange"

applyFirstChange (f:fs) (x:xs) = case f x of

    UnChanged _ -> (x:) <$> applyFirstChange fs xs

    change -> change <&> (:xs)