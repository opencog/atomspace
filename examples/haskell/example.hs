{-# LANGUAGE GADTs #-}

import OpenCog.AtomSpace        (AtomSpace,insert,get,remove,
                                 debug,runOnNewAtomSpace,showAtom,
                                 Atom(..),TruthVal(..),AtomGen(..))
import Control.Monad.IO.Class   (liftIO)

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = let a = AndLink (ConceptNode "John" Nothing)
                          (ConceptNode "Carlos" Nothing)
                          (Just $ SimpleTV 0.5 0.5)
           in do
        liftIO $ putStrLn "Let's insert some new nodes:"
        liftIO $ showAtom $ ConceptNode "Tall" Nothing
        insert $ ConceptNode "Tall" Nothing
        insert a
        liftIO $ showAtom a
        liftIO $ putStrLn "-----------After Insert:----------------"
        debug
        liftIO $ putStrLn "----------------------------------------"
        n <- get a
        case n of
          Just at -> liftIO $ putStrLn "AndLink found:" >> showAtom at
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
                            , AtomGen $ ConceptNode "hello" Nothing
                            , AtomGen $ NumberNode 4]
        insert list
        liftIO $ putStrLn "Inserted:"
        liftIO $ showAtom list

