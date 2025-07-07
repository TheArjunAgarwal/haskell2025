-- {-#OPTIONS_GHC -Wall#-}
{-# LANGUAGE LambdaCase #-}
import System.IO (readFile')
import System.Environment (getArgs)
import Data.List (partition)

main :: IO ()
main =
    writeFile "unlit_out.hs" .
    concatMap show .
    uncurry (++) .
    partition (("top"`elem`).words.language) .
    parser 0 Nothing .
    ( '\n' : ) =<<
    readFile' .
    head =<<
    getArgs

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

parser :: Integer -> Maybe ( CodeBlock String ) -> String -> [ CodeBlock String ]
parser line Nothing = \case
    '\n':'`':'`':'`':cs -> let ( language , rest ) = span (/='\n') cs in
                           parser (line + 1)
        ( Just ( CodeBlock { lineNumber = line + 1, language = language, code = "" } ) )
                                                                                rest
    '\n':cs             -> parser (line + 1)               Nothing              cs
    c   :cs             -> parser    line                  Nothing              cs
    ""                  -> []
parser line (Just codeBlock) = \case
    '\n':'`':'`':'`':cs -> codeBlock :
                           parser (line + 1)               Nothing              cs
    '\n':cs             -> parser (line + 1) ( Just ( ('\n':) <$> codeBlock ) ) cs
    c   :cs             -> parser    line    ( Just ( (  c :) <$> codeBlock ) ) cs
    ""                  -> []

data CodeBlock a = CodeBlock {
    lineNumber :: Integer ,
    language :: String ,
    code :: a
    }
    deriving Functor

instance Show (CodeBlock String) where
    show :: CodeBlock String -> String
    show =
        (
            ifThenElse . any(`elem`["hs","haskell"]).words<$>language <*>
                pure id <*>
                pure ( unlines . map ("-- "++) . lines )
        ) <*> (
            ( pure "\n\n-- line number : " <++> ( show <$> lineNumber ) ) <++>
            ( pure "\n-- language : " <++> language ) <++>
            ( reverse . code ) <++>
            pure "\n\n"
        )

ifThenElse :: Bool -> a -> a -> a
ifThenElse = \case
    True -> const
    False -> const id

data TextExtract = TextExtract {
    lineNumber' :: Integer ,
    textExtract :: String
}

instance Show TextExtract where
    show :: TextExtract -> String
    show = pure "\n\nline number : " <++> ( show <$> lineNumber' ) <++> textExtract <++> pure "\n"

deepBackTicks :: Integer -> [Char] -> [TextExtract]
deepBackTicks line = \case
    '\n':'`':'`':'`':cs -> deepBackTicks (line + 1) cs
    '\n':cs             -> deepBackTicks (line + 1) cs
    '`':'`':'`':cs      -> TextExtract { lineNumber' = line, textExtract = "```" ++ take 100 cs } : 
                           deepBackTicks    line    cs
    c:cs                -> deepBackTicks    line    cs
    ""                  -> []