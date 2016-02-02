-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE DataKinds #-}

-- | Simple example on using the Pattern Matcher.
import OpenCog.AtomSpace        (AtomSpace,insert,get,remove,cogBind,
                                 debug,runOnNewAtomSpace,printAtom,
                                 Atom(..),TruthVal(..),noTv,stv)
import Control.Monad.IO.Class   (liftIO)

findAnimals :: Atom
findAnimals = Link "BindLink"
                  [Node "VariableNode" "$var" noTv
                  ,Link "InheritanceLink"
                      [Node "VariableNode" "$var" noTv
                      ,Node "ConceptNode" "animal" noTv
                      ] noTv
                  ,Node "VariableNode" "$var" noTv
                  ] noTv

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = do
        insert $ Link "InheritanceLink"
                   [Node "ConceptNode" "fox" noTv
                   ,Node "ConceptNode" "animal" noTv
                   ] noTv
        insert $ Link "InheritanceLink"
                   [Node "ConceptNode" "cat" noTv
                   ,Node "ConceptNode" "animal" noTv
                   ] noTv
        insert findAnimals
        res <- cogBind findAnimals
        liftIO $ putStrLn "Result: " >> case res of
              Nothing       -> print res
              Just at -> printAtom at
        liftIO $ putStrLn "-----AtomSpace state at the end:-----"
        debug
        liftIO $ putStrLn "-------------------------------------"
