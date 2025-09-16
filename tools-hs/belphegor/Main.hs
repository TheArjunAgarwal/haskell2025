import System.Environment (getExecutablePath)
import FunDef ( parseFunDefs )
import System.IO (readFile')
import Normalize (normalize)
import Typst
import Expression (varE)
import System.FilePath (takeDirectory)
import Change ()
import System.Directory (withCurrentDirectory, getCurrentDirectory)
import qualified Data.Map as M
import Data.Bifunctor (bimap)

main :: IO ()
main = do
    exeDir <- getCurrentDirectory
    withCurrentDirectory exeDir $ do
        funDefs <-                                           M.map ( map $ bimap ( map $ fmap ( toContent . Raw ) ) ( fmap ( toContent . Raw ) )  ) . M.mapKeysMonotonic ( toContent . Raw ).
            parseFunDefs <$> readFile' "in.hs"
        let out =                                            map ( appendLnBrk . toContent ) .
                normalize funDefs                            . varE . toContent . Raw $
                "belphegorsExpression"
        writeFile "out.typ" "#import \"../../Modules/Tree.typ\" : tree \n "
        mapM_ ( appendFile "out.typ" . unWrap ) out