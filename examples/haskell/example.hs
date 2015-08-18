-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs #-}

-- | Simple example on inserting and removing many atoms in a new AtomSpace.
import OpenCog.AtomSpace        (AtomSpace,insert,get,remove,
                                 debug,runOnNewAtomSpace,printAtom,(|>),(\>),
                                 Atom(..),TruthVal(..),Gen(..),noTv,stv)
import Control.Monad.IO.Class   (liftIO)

main :: IO ()
main = runOnNewAtomSpace program

program :: AtomSpace ()
program = let a = AndLink (stv 0.5 0.5)
                    |> ConceptNode "John" noTv
                    \> ConceptNode "Carlos" noTv
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
          Just (AndLink _ _) -> liftIO $ putStrLn "AndLink found:"
          Nothing            -> liftIO $ putStrLn "No AndLink found."
        let list = ListLink [ Gen $ NumberNode 4
                            , Gen $ ConceptNode "hello" noTv
                            , Gen $ NumberNode 4]
        insert list
        liftIO $ putStrLn "Inserted:"
        liftIO $ printAtom list

