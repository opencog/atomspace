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

