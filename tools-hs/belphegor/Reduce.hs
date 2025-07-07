{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}

module Reduce (
    reduce,
    FunDefs,
    Expression
) where

import Expression ( 
    Expression(..), 
    VarDecls, 
    subVars 
    )

import FunDef (
    FunDef,
    FunDefs
    )
import qualified Data.Map as M (
    empty, 
    fromList, 
    unions, 
    lookup
    )
import Control.Monad (
    guard,
    zipWithM 
    )
import Change (
    Change,
    applyFirstChange,
    showChange, 
    ShowChange
    )
import Pattern (Pattern(..))

-- | apply one step of reduction rules
reduce :: forall a . (Show a, Ord a,ShowChange (Expression a) ) => FunDefs a -> 
    
    Expression a -> Change ( Expression a )

reduce funDefs (ConsE cons expressions) =

    ConsE cons <$> applyFirstChange ( repeat $ reduce funDefs ) expressions

reduce funDefs expression@Fun{name=function} = flip (maybe (error ("no definition for "++show function++" function")))

    (M.lookup function funDefs) (`applyDef` expression)

    where

    -- | apply the definition of a function
    applyDef :: FunDef a -> Expression a -> Change ( Expression a )

    applyDef [] x = error ( "incomplete patterns in definition of "++show (name x) )

    applyDef ( ( patterns , output ) : defsForOtherPatternss ) f@Fun{ name = fun , args = inputs } = 
        
        case ConsP fun patterns `match` ConsE { name = fun, args = inputs } of

            Nothing -> applyDef defsForOtherPatternss f

            Just ( Left change ) -> Fun fun . args <$> change

            Just ( Right varDecls ) -> showChange f (subVars varDecls output)

    applyDef _ c@ConsE{} = error ( "Impossible! trying to \"apply defininition\" of constructor "++show (name c) )

    -- | pattern `match` anExpression and get the values of the varibles in the pattern as a result \ 
    -- @Nothing@ means that match is definitely false \ 
    -- @Left change@ means that further evaluation was required, and therefore the first step of that further evaluation was carrried out
    match ::

        Pattern a -> Expression a ->

            Maybe (
                Either
                    ( Change ( Expression a ) )
                    ( VarDecls a )
                )

    WildCard `match` _ =                                                                                               pure.pure$

        M.empty -- a WildCard creates no variable declarations

    (VarP var) `match` xprsn =                                                                                         pure.pure$

        M.fromList [(var,xprsn)] -- obtained the declaration for the given variable

    (ConsP consP patterns) `match` (ConsE consE expressions) =                                                         (

        guard ( consP == consE )  {- if the constructors don't match, then match definitely fails -}                 >> ) . ( <$>

        zipWithM match patterns expressions                                                                          ) $ \ changesOrVarDecls ->

            case sequence changesOrVarDecls of

                Left _ -> {- which means at least one change WAS required -}                                         Left$
                    ConsE consE                                                                                      <$>
                        applyFirstChange {- so apply ony the first of those changes -}                               ( either pure (pure pure) <$>
                            changesOrVarDecls                                                                        )
                            expressions

                Right varDecls ->  {- which means all required variable declarations were obtained -}                pure$
                    M.unions varDecls

    match (ConsP _ _) xprsn@Fun{} =                                                                                  pure.Left$

        reduce funDefs xprsn