{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Tokens ( 
    splitIntoTokens , 
    codeLines 
) where

import Data.List (unfoldr, groupBy)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)))

data Token = Paren | Space | Chr deriving Eq

getToken :: Char -> Token
getToken c
    -- | (   ==   ';'  ) c = LnBrk
    | ( `elem` "()" ) c = Paren
    |     isSpace     c = Space
    |       otherwise   = Chr

toBeJoined :: Token -> Token -> Bool
toBeJoined Chr Chr = True
toBeJoined _ _ = False

splitIntoTokens :: String -> [String]
splitIntoTokens = filter (not.all isSpace) . groupBy (toBeJoined `on` getToken)

codeLines :: String -> [String]
codeLines = filter (not.all isSpace) . unfoldr maybeBreak where
    maybeBreak "" = Nothing
    maybeBreak (c:cs) = Just . break_ $ c :| cs
    break_ ('\n':|c :cs ) | isSpace c = break_ (c:|cs)
    break_ ('\n':|cs    )             = ("",cs)
    break_ (  c :|""    )             = ([c],"")
    break_ (  c :|c':c's)             = first (c:) . break_ $ c':|c's