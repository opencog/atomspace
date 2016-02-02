-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs #-}

-- | Simple example on creating different atomspaces and working with them.
import OpenCog.AtomSpace        (AtomSpace,Atom(..),insert,remove,newAtomSpace,
                                 getParent,debug,(<:),noTv,stv)

main :: IO ()
main = do
    parentAs <- newAtomSpace Nothing
    childAs <- newAtomSpace $ Just parentAs

    parentAs <: do
        insert $ Node "ConceptNode" "GenConcept1" (stv 1 1)
        insert $ Node "ConceptNode" "GenConcept2" (stv 1 1)
    childAs <: do
        insert $ Node "ConceptNode" "PrivateConcept1" noTv
        insert $ Node "ConceptNode" "PrivateConcept2" noTv

    case getParent childAs of
        Just parent -> if parentAs == parent
            then putStrLn "Proper Parent relation."
            else putStrLn "Error in Parent relation."
        Nothing     -> putStrLn "Error in Parent relation."

    putStrLn $ replicate 60 '#'
    putStrLn "State after inserting GenConcepts to Parent Atomspace"
    putStrLn "and PrivateConcepts to Child AtomSpace:"
    putStrLn "- Parent AtomSpace:"
    parentAs <: debug
    putStrLn "- Child AtomSpace:"
    childAs <: debug

    putStrLn $ replicate 60 '#'
    putStrLn "State after removing GenConcept1 from Parent Atomspace:"
    parentAs <: remove $ Node "ConceptNode" "GenConcept1" (stv 1 1)
    putStrLn "- Parent AtomSpace:"
    parentAs <: debug
    putStrLn "- Child AtomSpace:"
    childAs <: debug

    putStrLn $ replicate 60 '#'
    putStrLn "State after trying to remove GenConcept2 from Child Atomspace:"
    childAs <: remove $ Node "ConceptNode" "GenConcept2" noTv
    putStrLn "- Parent AtomSpace:"
    parentAs <: debug
    putStrLn "- Child AtomSpace:"
    childAs <: debug

    putStrLn $ replicate 60 '#'
    putStrLn "State after removing PrivateConcept1 from Child Atomspace:"
    childAs <: remove $ Node "ConceptNode" "PrivateConcept1" noTv
    putStrLn "- Parent AtomSpace:"
    parentAs <: debug
    putStrLn "- Child AtomSpace:"
    childAs <: debug

