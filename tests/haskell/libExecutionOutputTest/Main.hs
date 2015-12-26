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
import System.Directory
import System.Exit
import Data.Typeable

main :: IO ()
main = do
    res <- executionOutputTest
    case res of
        True -> exitSuccess
        False -> exitFailure

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

