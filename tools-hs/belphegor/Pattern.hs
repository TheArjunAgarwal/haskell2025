module Pattern(
    Pattern(..),
    parsePattern
) where

import Expression (Expression (..), parseExpression)
import Bracket (Bracketed)
-- module Pattern where

-- | pattern
data Pattern a =
    -- | WildCard in a pattern, matches anything
    WildCard |
    -- | variable in a pattern, matches anything and holds that value
    VarP a |
    -- | data constructor name and its args in a pattern
    ConsP a [Pattern a]

    deriving Functor

instance Show a => Show ( Pattern a ) where
    show :: Show a => Pattern a -> String
    show WildCard = "_"
    show (VarP var) = show var
    show (ConsP cons []) = show cons
    show (ConsP cons patterns) = ("("++) . (++")") . unwords $ show cons : ( show <$> patterns )

x2p :: Expression String -> Pattern String

x2p ( Fun  "_"  [  ]) = WildCard
x2p ( Fun  var  [  ]) = VarP var
x2p (ConsE cons args) = ConsP cons $ x2p <$> args

x2p x@(Fun _ (_:_)) = error ( "non-constructor function appearing in pattern " ++ show x )

parsePattern :: Bracketed String -> Pattern String
parsePattern = x2p . parseExpression