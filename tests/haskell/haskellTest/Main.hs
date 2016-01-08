{-# LANGUAGE TypeOperators,DataKinds,TypeFamilies#-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE Rank2Types             #-}

module Main where

import OpenCog.AtomSpace
import OpenCog.Test
import Data.Typeable
import GHC.Exts
import System.Directory
import System.Exit

main :: IO ()
main = do
    testres1 <- testInsertGet
    testres2 <- executionOutputTest
    case testres1 && testres2 of
        True -> exitSuccess
        False -> exitFailure

testInsertGet :: IO (Bool)
testInsertGet = do
    test <- mapM insertGet testData
    return $ and test

insertGet :: Gen AtomT -> IO Bool
insertGet a = do
    let prog = genInsert a >> genGet a
    res <- runOnNewAtomSpace prog
    case res of
        Just na -> return (na == a)
        Nothing -> error $ "Test Failed for atom: " ++ show a

executionOutputTest :: IO Bool
executionOutputTest = do
    curdir <- getCurrentDirectory
    let atom = ExecutionOutputLink
                (GroundedSchemaNode $ "lib: " ++ curdir ++
                    "/libopencoglib-0.1.0.0.so\\someFunc")
                (ListLink \> (ConceptNode "test" noTv))
        prog = insert atom >> execute atom
    res <- runOnNewAtomSpace prog
    case res of
        Just (Gen (ListLink [Gen (ConceptNode "test" _)])) -> return True
        Just ares -> error $ "ExecutionOutputTest failed and returned: " ++ show ares
        Nothing ->error $ "ExecutionOutputTest failed and retruned Nothing"
