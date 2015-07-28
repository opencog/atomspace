{-# LANGUAGE GADTs #-}

import OpenCog.AtomSpace            (TruthVal(..),Atom(..),AtomGen(..),
                                     runOnNewAtomSpace,get,insert,remove,
                                     printAtom)
import Control.Monad.IO.Class       (liftIO)

someTv :: Maybe TruthVal
someTv = Just $ SimpleTV 0.4 0.5

n = ConceptNode "Animal" someTv
l = ListLink [AtomGen n]

e = EvaluationLink
   (PredicateNode "isFriend")
   (ListLink [ AtomGen $ ConceptNode "Alan" Nothing
             , AtomGen $ ConceptNode "Robert" Nothing
             ])
   someTv

li = (ListLink [ AtomGen $ ConceptNode "SomeConcept" someTv
               , AtomGen $ PredicateNode "SomePredicate"
               ])

-- Type checking Ok.
ex1 = ExecutionLink
          (GroundedSchemaNode "some-fun")
          (ListLink [ AtomGen $ ConceptNode "Arg1" someTv
                    , AtomGen $ ConceptNode "Arg2" someTv
                    ])
          (ConceptNode "res" someTv)

{- Type checking error.
ex2 = ExecutionLink
          (ConceptNode "some-fun" someTv) -- ConceptNodeT type isn't instance of IsSchema
          (ListLink [ AtomGen $ ConceptNode "Arg1" someTv
                    , AtomGen $ ConceptNode "Arg2" someTv
                    ])
          (ConceptNode "res" someTv)
-}

main :: IO ()
main = runOnNewAtomSpace $ do
         p <- get $ PredicateNode "Pred"
         case p of
            Just (PredicateNode _) -> liftIO $ print "Predicate found."
            _                      -> liftIO $ print "No Predicate found."
         liftIO $ printAtom li
         () <- case li of
           ListLink (x:_) -> case x of
               AtomGen (ConceptNode c _)  -> liftIO $ print "First is Concept"
               AtomGen (PredicateNode p ) -> liftIO $ print "First is Predicate"
               _                          -> liftIO $ print "First is other type"
         insert e
         get e
         remove e
         return ()

