-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE DataKinds #-}

-- | Simple example on inserting and removing many atoms in a new AtomSpace.
import OpenCog.AtomSpace         (TruthVal(..),Atom(..),AtomType(..),Gen(..),
                                  AtomSpace,runOnNewAtomSpace,get,insert,remove,
                                  debug,printAtom,noTv,stv,(|>),(\>))
import Control.Monad.IO.Class    (liftIO)

conceptN :: Int -> Atom ConceptT
conceptN i = ConceptNode ("Animal"++ show i) (stv 1 1)

main :: IO ()
main = runOnNewAtomSpace $ do
         let listConcepts :: Int -> [Atom ConceptT]
             listConcepts n = [conceptN i | i <- [1..n]]
         mapM insert $ listConcepts 50000
         res <- get $ conceptN 49877
         liftIO $ putStrLn $ "We found: " ++ show res
         mapM remove $ listConcepts 50000
         debug
         insert $ ListLink
                    |> (ListLink $ map Gen $ listConcepts 200)
                    \> (ListLink $ map Gen $ listConcepts 200)
         debug
         return ()

