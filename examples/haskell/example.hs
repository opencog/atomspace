{-# LANGUAGE GADTs #-}

import OpenCog.AtomSpace        (AtomSpace,insert,get,remove,
                                 debug,runOnNewAtomSpace,printAtom,
                                 Atom(..),TruthVal(..),AtomGen(..),noTv,withTv)
import Control.Monad.IO.Class   (liftIO)

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = let a = AndLink (withTv $ SimpleTV 0.5 0.5)
                          (ConceptNode "John" noTv)
                          (ConceptNode "Carlos" noTv)
           in do
        liftIO $ putStrLn "Let's insert some new nodes:"
        liftIO $ printAtom $ ConceptNode "Tall" noTv
        insert $ ConceptNode "Tall" noTv
        insert a
        liftIO $ printAtom a
        liftIO $ putStrLn "-----------After Insert:----------------"
        debug
        liftIO $ putStrLn "----------------------------------------"
        n <- get a
        case n of
          Just at -> liftIO $ putStrLn "AndLink found:" >> printAtom at
          Nothing -> liftIO $ putStrLn "No AndLink found."
        remove a
        liftIO $ putStrLn "-----------After Remove:----------------"
        debug
        liftIO $ putStrLn "----------------------------------------"
        n <- get a
        case n of
          Just (AndLink _ _ _) -> liftIO $ putStrLn "AndLink found:"
          Nothing              -> liftIO $ putStrLn "No AndLink found."
        let list = ListLink [ AtomGen $ NumberNode 4
                            , AtomGen $ ConceptNode "hello" noTv
                            , AtomGen $ NumberNode 4]
        insert list
        liftIO $ putStrLn "Inserted:"
        liftIO $ printAtom list

