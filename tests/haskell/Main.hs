{-# LANGUAGE TypeOperators,DataKinds,TypeFamilies#-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TemplateHaskell        #-}
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

setDepth (SmallCheckDepth d) = (SmallCheckDepth 4)

suite :: TestTree
suite = adjustOption setDepth $ testGroup "Haskell Test-Suite" [
    testProperty "simple Insert Test" testInsert]

testInsert :: (Gen AtomT) -> Property IO
testInsert a = monadic $ do
    print a
    putStrLn "------------------------------------"
    runOnNewAtomSpace $ do
        (appGen insert a :: AtomSpace ())
        ma <- genGet a
        case ma of
            Just na -> return True
            Nothing -> return False

genGet :: Gen AtomT -> AtomSpace (Maybe (Gen AtomT))
genGet (Gen a) = do
    res <- get a
    case res of
        Just x ->  return $ Just $ Gen x
        Nothing -> return $ Nothing

--prop_InsertP a@(PredicateNode _ _) = monadicIO $ do
