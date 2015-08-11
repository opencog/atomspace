
import OpenCog.AtomSpace            (Atom(..),AtomGen(..),AtomSpace,appGen,cogBind,
                                     runOnNewAtomSpace,get,insert,remove,debug,
                                     printAtom,AtomType(..),noTv,stv,(|>),(\>))
import Control.Monad.IO.Class       (liftIO)
import BackgroundKnowledge          (bkn)
import MosesModel                   (mosesModel)

main :: IO ()
main = let insertGen :: AtomGen -> AtomSpace ()
           insertGen = appGen insert
           printGen :: AtomGen -> IO ()
           printGen = appGen printAtom
        in runOnNewAtomSpace $ do
                mapM_ insertGen bkn
                insert mosesModel
                debug

