
import OpenCog.AtomSpace.Api    (AtomSpace,asAddNode,runOnNewAtomSpace)
import OpenCog.AtomSpace.Types  (Node(..),NodeType(..))
import Control.Monad.IO.Class   (liftIO)
import Data.Default             (Default(..))

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = do
              liftIO $ putStrLn "Let's add some new nodes:"
              h1 <- asAddNode def{ nodeName = "NewNode"
                                 , nodeType = ConceptNode }
              h2 <- asAddNode def{ nodeName = "AnotherNewNode"
                                 , nodeType = ConceptNode}
              liftIO $ putStrLn $ "Added with handles "
                                  ++ show(h1) ++ " and " ++ show(h2)

