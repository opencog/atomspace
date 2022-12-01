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
import Data.Typeable
import GHC.Exts
import System.Directory
import System.Exit

main :: IO ()
main = do
    testres1 <- executionOutputTest
    testres2 <- evaluationTest
    case testres1 && testres2 of
        True -> exitSuccess
        False -> exitFailure
        
executionOutputTest :: IO Bool
executionOutputTest = do
    curdir <- getCurrentDirectory
    let atom = Link "ExecutionOutputLink"
                [Node "GroundedSchemaNode" ("lib: " ++ curdir ++
                    "/libopencoglib-0.1.0.0.so\\someFunc") noTv
                ,Link "ListLink" [Node "ConceptNode" "test" noTv] noTv
                ] noTv
        prog = insert atom >> execute atom
    res <- runOnNewAtomSpace prog
    case res of
        Just (Link "ListLink" [Node "ConceptNode" "test" _] _) -> return True
        Just ares -> error $ "ExecutionOutputTest failed and returned: " ++ show ares
        Nothing ->error $ "ExecutionOutputTest failed and returned Nothing"

        
evaluationTest :: IO Bool
evaluationTest = do
    curdir <- getCurrentDirectory
    let atom = Link "EvaluationLink"
                [Node "GroundedPredicateNode" ("lib: " ++ curdir ++
                    "/libopencoglib-0.1.0.0.so\\somePredicate") noTv
                ,Link "ListLink" [Node "ConceptNode" "test" (SimpleTV 0.5 0.5)] noTv
                ] noTv
        prog = insert atom >> evaluate atom
    res <- runOnNewAtomSpace prog
    case res of
        Just (SimpleTV 0.5 0.5) -> return True
        _ -> error $ "EvaluationTest failed and returned: " ++ show res


myatom p = Link "ExecutionOutputLink"
                [Node "GroundedSchemaNode" ("lib: " ++ p ++
                    "/libopencoglib-0.1.0.0.so\\someFunc") noTv
                ,Link "ListLink" [Node "ConceptNode" "test" noTv] noTv
                ] noTv
