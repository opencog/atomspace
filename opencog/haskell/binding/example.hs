
import OpenCog.AtomSpace.Api
import OpenCog.AtomSpace.Types
import Control.Monad.IO.Class
import Data.Default

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = do
              liftIO $ putStrLn "Let's add some new nodes:"
              liftIO $ putStrLn "---Before:--"
              asPrint
              asAddNode def{nodeName="NewNode", nodeType = ConceptNode}
              asAddNode def{nodeName="AnotherNewNode", nodeType = ConceptNode}
              liftIO $ putStrLn "---After:---"
              asPrint
              liftIO $ putStrLn "------------"

