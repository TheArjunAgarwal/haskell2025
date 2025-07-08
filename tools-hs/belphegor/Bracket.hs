--  unfoldr f = fix $ \ r -> maybe [] ( uncurry (:) . fmap r ) . f 
--  unfoldr = fix.(.maybe[].(uncurry(:).).fmap).flip(.)
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

module Bracket (
    Bracketed(..),
    bracketsParser
) where

import Data.List.NonEmpty (NonEmpty((:|)))

data Bracketed a = Word a | Bracketed [Bracketed a] deriving (Eq,Show,Functor)

-- brackets parser combinator for foldr
bracketsParserCombinator ::
    String
    -> NonEmpty [ Bracketed String ]
    -> NonEmpty [ Bracketed String ]

bracketsParserCombinator ")" ( x :|    xs   ) = []                   :| x : xs

bracketsParserCombinator "(" ( x :| x' : xs ) = ( Bracketed x : x' ) :|   xs
bracketsParserCombinator "("        l         = error ( "too many '(' before " ++ show l )

bracketsParserCombinator  w  ( x :|    xs   ) = (      Word w : x  ) :|   xs

-- | given a line of tokens, parse the bracket structure 
bracketsParser :: [String] -> [Bracketed String]
bracketsParser =
    \case
        parsedWords :| [] -> parsedWords
        parsedWords :| _  -> error ( "too many ')' after " ++ show parsedWords ) 
    . foldr bracketsParserCombinator ([]:|[])
    -- . split

{-
ghci> a = tail . init . filter (`elem` "[]") . show . bp $ " ( ( ))  ( ) (( )())  ()"
ghci> b = filter (`elem` "()") $ " ( ( ))  ( ) (( )())  ()"
ghci> mapM_ putStrLn [a,b]
-}


-- pp :: Bracketed String -> Pattern String
-- -- redundant bracket
-- pp (Bracketed [pattern]) = pp pattern
-- -- unit
-- pp 
--     (
--         Bracketed []
--     ) 
--     = ConsP "()" []
-- -- constant 
-- pp 
--     (
--         Word w@(c:_)
--     ) 
--     | isUpper c || strIsNumber w
--     = ConsP w $ pp <$> []
-- -- symbolic constant
-- pp 
--     (
--         Word w@(':':_)
--     ) 
--     = ConsP w $ pp <$> []
-- -- function
-- pp 
--     (
--         Bracketed 
--             ( 
--                 Word w@(c:_)
--                 : ws@(_:_)
--             )
--     ) 
--     | isUpper c || w == "(,)"
--     = ConsP w $ pp <$> ws
-- -- symbolic function
-- pp 
--     (
--         Bracketed 
--             ( 
--                 Bracketed [ Word w@(':':_) ]
--                 : ws@(_:_)
--             )
--     ) 
--     = ConsP w $ pp <$> ws
-- -- pairing function 
-- pp 
--     (
--         Bracketed 
--             (
--                 Bracketed [Word ","]
--                 : ws@(_:_)
--             )
--     )
--     = ConsP "(,)" $ pp <$> ws
-- -- WildCard
-- pp (Word "_") = WildCard
-- -- variable
-- pp (Word w) = VarP w
-- -- variable function 
-- pp 
--     (
--         Bracketed 
--             ( 
--                 Word w
--                 : ws@(_:_) 
--             )
--     ) 
--     =  error ( "variable " ++ show w ++ " applied to args : " ++ show ws )
-- -- too many brackets
-- pp 
--     x@(
--         Bracketed 
--             ( 
--                 (Bracketed _) 
--                 : (_:_) 
--             )
--     ) 
--     = error ("Parse error in pattern : " ++ show x)