{-# LANGUAGE GADTs #-}

import OpenCog.AtomSpace            (TruthVal(..),Atom(..),Gen(..),
                                     runOnNewAtomSpace,get,insert,remove,
                                     printAtom,noTv,stv,(|>),(\>))
import Control.Monad.IO.Class       (liftIO)

n = ConceptNode "Animal" (stv 1 1)

l = ListLink \> n

e = EvaluationLink noTv
   (PredicateNode "isFriend" noTv)
   (ListLink |> ConceptNode "Alan" noTv
             \> ConceptNode "Robert" noTv )

li = ListLink |> ConceptNode   "SomeConcept" (stv 1 1)
              |> PredicateNode "SomePredicate" noTv
              |> PredicateNode "SomePredicate" noTv
              \> ListLink |> ConceptNode   "SomeConcept" (stv 1 1)
                          |> PredicateNode "SomePredicate" noTv
                          \> PredicateNode "SomePredicate" noTv

-- Type checking Ok.
ex1 = ExecutionLink
          (GroundedSchemaNode "some-fun")
          (ListLink |> ConceptNode "Arg1" (stv 1 1)
                    \> ConceptNode "Arg2" (stv 1 1) )
          (ConceptNode "res" (stv 1 1))

{- Type checking error.
ex2 = ExecutionLink
          (ConceptNode "some-fun" (stv 1 1)) -- ConceptNodeT isn't a Schema.
          (ListLink |> ConceptNode "Arg1" (stv 1 1)
                    \> ConceptNode "Arg2" (stv 1 1) )
          (ConceptNode "res" (stv 1 1))
-}

main :: IO ()
main = runOnNewAtomSpace $ do
         p <- get $ PredicateNode "Pred" noTv
         case p of
            Just (PredicateNode _ _) -> liftIO $ print "Predicate found."
            _                        -> liftIO $ print "No Predicate found."
         liftIO $ printAtom li
         () <- case li of
           ListLink (x:_) -> case x of
               Gen (ConceptNode c _)    -> liftIO $ print "First is Concept"
               Gen (PredicateNode p _ ) -> liftIO $ print "First is Predicate"
               _                        -> liftIO $ print "First is other type"
         insert e
         get e
         remove e
         return ()

