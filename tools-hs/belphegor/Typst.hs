module Typst where
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List (intercalate)

class Content a where
    toContent :: a -> ContentWrapper

newtype ContentWrapper = Wrap { unWrap::String } deriving (Show,Eq,Ord)

instance Content ContentWrapper where
    toContent :: ContentWrapper -> ContentWrapper
    toContent = id

highlight :: ContentWrapper -> ContentWrapper
highlight = Wrap <$> ( ("#box(stroke:red,inset:2pt)["++).(++"]") . unWrap )

appendLnBrk :: ContentWrapper -> ContentWrapper
appendLnBrk = Wrap <$> ( (++" \\ \n") . unWrap )

newtype Raw = Raw String deriving (Eq, Ord, Show)

instance Content Raw where
    toContent :: Raw -> ContentWrapper
    toContent (Raw str) = Wrap ( "`"++str++"`" )

newtype Tree a = ParentAndSubtrees ( a , [ Tree a ] )

instance Content a => Content ( Tree a ) where
    toContent :: Content a => Tree a -> ContentWrapper
    toContent = Wrap . ("#tree("++).(++")") . help where
        help ( ParentAndSubtrees (p,ts) ) = 
            ("("++).(++")") . concatMap (++",") $ ( ("["++).(++"]") . unWrap . toContent $ p ) : ( help <$> ts )