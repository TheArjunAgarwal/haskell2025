{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Expression(
    Expression(..),
    parseExpression,
    varE,
    apply,
    VarDecls,
    subVars
) where

import qualified Data.Map as M ( Map, findWithDefault )
import Bracket ( Bracketed(..) )
import Data.Char ( isUpper )
import Text.Read ( readMaybe )
import Data.Maybe ( isJust )
import Typst 
import Change 

-- | Well-formed expressions, \ 
-- Note that variables are functions with no args
data Expression a =
    -- | a function name and its args
    Fun { name::a , args::[Expression a] } |
    -- | data constructor name and its args in an expression
    ConsE { name::a , args::[Expression a] }

    deriving Functor
    
-- | make an expression which is variable, i.e., a function with no args
varE :: a -> Expression a
varE a = Fun { name = a, args = [] }

instance Show a => Show ( Expression a ) where
    show :: Show a => Expression a -> String
    show x = case args x of
        [] -> show $ name x
        _ -> unwords $ show ( name x ) : ( ("("++) . (++")") . show <$> args x )

instance Content a => Content ( Expression a ) where
    toContent :: Content a => Expression a -> ContentWrapper
    toContent = toContent . help where
        help x = ParentAndSubtrees ( name x , help <$> args x)

instance ShowChange (Expression ContentWrapper) where
    customizationOfBefore :: Expression ContentWrapper -> Expression ContentWrapper
    customizationOfBefore x = x { name = highlight . name $ x }

parseExpression :: Bracketed String -> Expression String

-- unit
parseExpression ( Bracketed [] )                           = ConsE { name =      "()"       , args = [] }
-- pairing function
parseExpression ( Bracketed [ Word "," ] )                 = ConsE { name =      "(,)"      , args = [] }
-- constructor 
parseExpression ( Word w ) | strIsConstructorFunction w    = ConsE { name =        w        , args = [] }
-- constructor operator
parseExpression( Bracketed [ Word w@( ':' : _ ) ] )        = ConsE { name = "(" ++ w ++ ")" , args = [] }

-- variable
parseExpression ( Word w )                                 = varE                  w
-- variable operator
parseExpression ( Bracketed [ Word w ] ) | strIsOperator w = varE  (        "(" ++ w ++ ")"             )

-- application
parseExpression ( Bracketed ( w : ws ) )                   = parseExpression w `apply` ( parseExpression <$> ws )

strIsConstructorFunction "[]" = True
strIsConstructorFunction w@(c:_) 
               | isUpper c = True
strIsConstructorFunction w = isJust ( readMaybe w :: Maybe Integer ) 

strIsOperator :: Foldable t => t Char -> Bool
strIsOperator = all ( `elem` ">+~!%:-=*/^\\$&|<@" )

-- | given an expression @e@ and some expressions @es@, \ 
-- treat @e@ as a function and apply it to the arguments @es@
apply :: Expression a -> [Expression a] -> Expression a
apply x xs = x { args = args x ++ xs}

-- | variable declarations is a correspondence between variable names and expressions 
type VarDecls a = M.Map a (Expression a)

-- | substitute the variables in an expression by their declared values
subVars :: Ord a =>

    VarDecls a -> Expression a -> Expression a

subVars varDecls expression = expressionDecl `apply` subbedArgs where

    expressionDecl = M.findWithDefault ( expression { args = [] } ) ( name expression ) varDecls
    
    subbedArgs = subVars varDecls <$> args expression 