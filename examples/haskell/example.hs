-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs #-}

-- | Simple example on inserting and removing many atoms in a new AtomSpace.
import OpenCog.AtomSpace        (AtomSpace,Atom(..),insert,get,remove,debug,
                                 runOnNewAtomSpace,printAtom,(|>),(\>),noTv,stv)
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
        showLine
        liftIO $ putStrLn "After Insert:"
        debug
        showLine
        n <- get a
        case n of
          Just at -> liftIO $ putStrLn "AndLink found:" >> printAtom at
          Nothing -> liftIO $ putStrLn "No AndLink found."
        remove a
        showLine
        liftIO $ putStrLn "After Remove:"
        debug
        showLine
        n <- get a
        case n of
          Just (AndLink _ _) -> liftIO $ putStrLn "AndLink found:"
          Nothing            -> liftIO $ putStrLn "No AndLink found."
        let list = ListLink |> NumberNode 4
                            |> ConceptNode "hello" noTv
                            \> NumberNode 4
        insert list
        liftIO $ putStrLn "Inserted:"
        liftIO $ printAtom list
    where
        showLine :: AtomSpace ()
        showLine = liftIO $ putStrLn $ replicate 60 '#'
