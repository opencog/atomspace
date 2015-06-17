{-# LANGUAGE GADTs #-}

import OpenCog.AtomSpace        (AtomSpace,insert,get,remove,
                                 debug,runOnNewAtomSpace,
                                 Atom(..),TruthVal(..),AtomGen(..))
import Control.Monad.IO.Class   (liftIO)

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = let andLink = AndLink (ConceptNode "John")
                                (ConceptNode "Carlos")
                                (Just $ SimpleTV 0.5 0.5)
           in do
        liftIO $ putStrLn "Let's insert some new nodes:"
        insert $ Concept "Tall"
        insert andLink
        liftIO $ putStrLn "-----------After Insert:----------------"
        debug
        liftIO $ putStrLn "----------------------------------------"
        n <- get andLink
        case n of
          Just (AndLink _ _ _) -> liftIO $ putStrLn $ (show n) ++ " found."
          Nothing              -> liftIO $ putStrLn "No AndLink found."
        remove andLink
        liftIO $ putStrLn "-----------After Remove:----------------"
        debug
        liftIO $ putStrLn "----------------------------------------"
        n <- get andLink
        case n of
          Just (AndLink _ _ _) -> liftIO $ putStrLn $ (show n) ++ " found."
          Nothing              -> liftIO $ putStrLn "No AndLink found."
        insert $ ListLink [ AtomGen $ NumberNode 4
                          , AtomGen $ ConceptNode "hello"
                          , AtomGen $ NumberNode 4]


