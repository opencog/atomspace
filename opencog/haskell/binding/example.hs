
import OpenCog.AtomSpace.Api
import OpenCog.AtomSpace.Types
import Data.Default

main :: IO ()
main = do 
          at <- atomspace_new
          putStrLn "Let's add some new nodes:"
          putStrLn "-------- Atomspace Before --------"
          atomspace_print at
          atomspace_addnode at def{node_name="NewNode", node_type = ConceptNode}
          atomspace_addnode at def{node_name="AnotherNewNode", node_type = ConceptNode}
          putStrLn "-------- Atomspace After  --------"
          atomspace_print at
          putStrLn "----------------------------------"
          atomspace_delete at

