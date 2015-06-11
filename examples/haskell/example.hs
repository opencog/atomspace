{-# LANGUAGE GADTs #-}

import OpenCog.AtomSpace.Api   (AtomSpace,insert,get,remove,
                                debug,runOnNewAtomSpace)
import OpenCog.AtomSpace.Types (Atom(..),TruthVal(..),AtomGen(..))
import Control.Monad.IO.Class  (liftIO)

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = let andLink = And (Concept "John")
                            (Concept "Carlos")
                            (Just $ SimpleTruthVal 0.5 0.5)
           in do
        liftIO $ putStrLn "Let's insert some new nodes:"
        insert $ Concept "Tall"
        insert andLink
        liftIO $ putStrLn "-----------After Insert:----------------"
        debug
        liftIO $ putStrLn "----------------------------------------"
        n <- get andLink
        case n of
          Just (And _ _ _) -> liftIO $ putStrLn "AndLink found."
          Nothing          -> liftIO $ putStrLn "No AndLink found."
        remove andLink
        liftIO $ putStrLn "-----------After Remove:----------------"
        debug
        liftIO $ putStrLn "----------------------------------------"
        n <- get andLink
        case n of
          Just (And _ _ _) -> liftIO $ putStrLn "AndLink found."
          Nothing          -> liftIO $ putStrLn "No AndLink found."
        insert $ List [ AtomGen $ Number 4
                      , AtomGen $ Concept "hello"
                      , AtomGen $ Number 4]


