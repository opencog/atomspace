{-# LANGUAGE GADTs #-}

import OpenCog.AtomSpace            (TruthVal(..),Atom(..),Gen(..),
                                     runOnNewAtomSpace,get,insert,remove,
                                     printAtom,withTv,noTv,(|>),(\>))
import Control.Monad.IO.Class       (liftIO)

someTv :: Maybe TruthVal
someTv = withTv $ SimpleTV 0.4 0.5

n = ConceptNode "Animal" someTv

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
          (ListLink |> ConceptNode "Arg1" someTv
                    \> ConceptNode "Arg2" someTv )
          (ConceptNode "res" someTv)

{- Type checking error.
ex2 = ExecutionLink
          (ConceptNode "some-fun" someTv) -- ConceptNodeT type isn't instance of IsSchema
          (ListLink [ Gen $ ConceptNode "Arg1" someTv
                    , Gen $ ConceptNode "Arg2" someTv
                    ])
          (ConceptNode "res" someTv)
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

