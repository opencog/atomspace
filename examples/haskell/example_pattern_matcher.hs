{-# LANGUAGE GADTs     #-}
{-# LANGUAGE DataKinds #-}

import OpenCog.AtomSpace        (AtomSpace,insert,get,remove,cogBind,
                                 debug,runOnNewAtomSpace,printAtom,
                                 Atom(..),AtomType(BindT),TruthVal(..),
                                 Gen(..),noTv,stv)
import Control.Monad.IO.Class   (liftIO)

findAnimals :: Atom BindT
findAnimals = BindLink
                  (VariableNode "$var")
                  (InheritanceLink noTv
                      (VariableNode "$var")
                      (ConceptNode "animal" noTv)
                  )
                  (VariableNode "$var")

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = do
        insert $ (InheritanceLink noTv
                    (ConceptNode "fox" noTv)
                    (ConceptNode "animal" noTv)
                 )
        insert $ (InheritanceLink noTv
                    (ConceptNode "cat" noTv)
                    (ConceptNode "animal" noTv)
                 )
        insert findAnimals
        res <- cogBind findAnimals
        liftIO $ putStrLn "Result: " >> case res of
              Nothing       -> print res
              Just (Gen at) -> printAtom at
        liftIO $ putStrLn "-----AtomSpace state at the end:-----"
        debug
        liftIO $ putStrLn "-------------------------------------"

