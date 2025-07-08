{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
{-# LANGUAGE LambdaCase #-}

module Normalize (
    normalize
) where

import Reduce (
    reduce,
    FunDefs,
    Expression
    )
import Change (
    Change(..), after, ShowChange
    )
import Data.List (
    unfoldr
    )

normalize :: (Show a, Ord a, ShowChange (Expression a)) => 
    FunDefs a -> Expression a -> [Change (Expression a)]
normalize funDefs = unfoldr ( \case { UnChanged _ -> Nothing ; change -> Just ( change , after change ) } . reduce funDefs )