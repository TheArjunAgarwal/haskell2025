{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
{-# LANGUAGE LambdaCase #-}

-- | function definition syntax and types
module FunDef (
    FunDef,
    FunDefs,
    parseFunDefs,
    preFunDef
) where

import Expression ( Expression (..), parseExpression )
import qualified Data.Map as M
import Bracket (Bracketed(..), bracketsParser)
import Data.List (unfoldr)
import Pattern (Pattern, parsePattern)
import Tokens (codeLines, splitIntoTokens)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (mapMaybe)


-- | a function definition is a correspondence between lists of patterns (the args declarations)
-- and the expressions which are to be returned
-- 
-- For example, the definition - 
--
-- > ifThenElse True  x _ = x  
-- > ifThenElse False _ y = y 
-- 
-- would be stored as - 
-- 
-- > [ 
-- >    ( [ Cons True  [] ,  VarP x  , WildCard ] , VarE x ) , 
-- >    ( [ Cons False [] , WildCard ,  VarP y  ] , VarE y ) 
-- > ]
type FunDef a = [ ( [Pattern a] , Expression a ) ]

-- | read a line of code as a function definition \ 
-- and return ( function name , ( LHS patterns , RHS expression ) )
preFunDef :: [Bracketed String] -> Maybe (String,([Bracketed String], Bracketed String))
preFunDef codeLine = case unfoldr maybeBreak codeLine of
    []    -> Nothing -- error "empty line"
    [_]   -> Nothing -- error ( "no '=' present in line " ++ show l )
    [p,e] -> Just .
        (\(a,b)->(a,(b,Bracketed e))) $
        case p of
            Bracketed [Word w] : ws -> ("("++w++")",ws)
            Word w             : ws -> (w,ws)
            _                       -> error ( "could not parse function name in" ++ show codeLine )
    l     -> error ( "too many '=' present in line " ++ show l )
    where
    maybeBreak [] = Nothing
    maybeBreak l = Just $ drop 1 <$> break (\case{Word"="->True;_->False}) l

-- | function names and their correspoding defintions
type FunDefs a = M.Map a (FunDef a)

parseFunDefs :: String -> FunDefs String
parseFunDefs =
    M.fromListWith (flip (++))                                              . fmap ( fmap ( pure .
    bimap ( map parsePattern ) parseExpression                              ) ) . mapMaybe (
    preFunDef                                                               .
    bracketsParser                                                          .
    splitIntoTokens                                                         ) . codeLines