-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE DataKinds #-}

-- | Simple example on executing code in multiple threads.
-- Note, before compiling this code you need to install the package: 'random'.
-- Executing: stack install random
import OpenCog.AtomSpace        (AtomSpace,insert,get,remove,
                                 debug,printAtom,newAtomSpace,(<:),
                                 Atom(..),TruthVal(..),noTv,stv)
import Control.Monad.IO.Class   (liftIO)
import Control.Concurrent       (forkIO,threadDelay)
import System.Random            (randomIO,randomRIO)

randomConcept :: Int -> AtomSpace Atom
randomConcept top = do
    num <- liftIO $ randomRIO (1,top)
    return $ Node "ConceptNode" ("Concept"++show num) noTv

randomList :: Int -> Int -> AtomSpace Atom
randomList n m = do
    num <- liftIO $ randomRIO (1,n)
    list <- mapM (\_ -> randomConcept m >>= return) [1..num]
    return $ Link "ListLink" list noTv

main :: IO ()
main = do
    as1 <- newAtomSpace Nothing
    mapM (\n -> forkIO $ as1 <: loop n) [1..20]
    as1 <: loop 21

loop :: Int -> AtomSpace ()
loop idNum = do
    liftIO $ putStrLn $ "Thread " ++ show idNum

    waitRandom

    concept1 <- randomConcept 6
    remove concept1

    concept2 <- randomConcept 6
    insert concept2

    concept3 <- randomConcept 6
    get concept3

    waitRandom

    list1 <- randomList 3 6
    res <- get list1
    case res of
        Nothing -> liftIO $ putStrLn "Got: Nothing"
        Just l  -> liftIO $ putStrLn "Got:" >> printAtom l

    list2 <- randomList 3 6
    insert list2

    list3 <- randomList 3 6
    remove list3

    if idNum == 1
      then do
        liftIO $ threadDelay 5000000
        liftIO $ putStrLn $ replicate 70 '#'
        debug
      else return ()

    loop idNum

  where
    waitRandom :: AtomSpace ()
    waitRandom = do
        n <- liftIO $ randomRIO (0,100000)
        liftIO $ threadDelay n
