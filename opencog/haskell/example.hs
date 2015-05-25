
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
              h1 <- asAddNode def{nodeName="NewNode", nodeType = ConceptNode}
              h2 <- asAddNode def{nodeName="AnotherNewNode", nodeType = ConceptNode}
              liftIO $ putStrLn $ "---Added with handles " ++ show(h1) ++ " and " ++ show(h2)
              liftIO $ putStrLn "---After:---"
              asPrint
              liftIO $ putStrLn "------------"

