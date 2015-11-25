{-# LANGUAGE TypeOperators,DataKinds,TypeFamilies#-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE Rank2Types             #-}

module Main where

import System.Exit
import OpenCog.AtomSpace
import OpenCog.Test
import Test.Tasty
import Test.Tasty.HUnit
import Data.Typeable
import GHC.Exts
import Control.Exception (throwIO)

import Debug.Trace

main :: IO ()
main = defaultMain suite

mytrace a = traceShow a a

suite :: TestTree
suite = testGroup "Haskell Test-Suite" [
    testCase "simple Insert Test" $ assert testInsertGet
    ]

testInsertGet :: IO (Bool)
testInsertGet = do
    test <- mapM insertGet testData
    return $ and test

insertGet :: Gen AtomT -> IO Bool
insertGet a = do
    let prog = genInsert a >> genGet a
    res <- runOnNewAtomSpace prog
    return True
    case res of
        Just na -> return (na == a)
        Nothing -> error $ "Test Failed for atom: " ++ show a
