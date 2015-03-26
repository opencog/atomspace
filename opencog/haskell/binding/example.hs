
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
              asAddNode def{node_name="NewNode", node_type = ConceptNode}
              asAddNode def{node_name="AnotherNewNode", node_type = ConceptNode}
              liftIO $ putStrLn "---After:---"
              asPrint
              liftIO $ putStrLn "------------"

