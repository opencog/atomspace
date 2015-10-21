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
import OpenCog.AtomSpace.AtomType
import OpenCog.Test
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Data.Typeable
import GHC.Exts
import Control.Exception

main :: IO ()
main = defaultMain suite

setDepth (SmallCheckDepth d) = SmallCheckDepth 4

suite :: TestTree
suite = adjustOption setDepth $ testGroup "Haskell Test-Suite" [
    testProperty "simple Insert Test" testInsertGet]

testInsertGet :: Gen AtomT -> Property IO
testInsertGet a = monadic $ do
    let prog = genInsert a >> genGet a
    res <- runOnNewAtomSpace prog
    case res of
        Just na -> print "-----" >> print na >> print a >> return (na == a)
        Nothing -> return False
    --ma <- genGet a
    --case ma of
    --    Just na -> return (na == a)
    --    Nothing -> return False

