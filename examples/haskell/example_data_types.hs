-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE DataKinds #-}

-- | Simple example on defining many atom types and testing the compiler type checking.
import OpenCog.AtomSpace         (TruthVal(..),Atom(..),AtomType(..),Gen(..),
                                  runOnNewAtomSpace,get,insert,remove,
                                  printAtom,noTv,stv,(|>),(\>))
import Control.Monad.IO.Class    (liftIO)

n :: Atom ConceptT
n = ConceptNode "Animal" (stv 1 1)

e :: Atom EvaluationT
e = EvaluationLink noTv
        (PredicateNode "isFriend" noTv)
        (ListLink |> ConceptNode "Alan" noTv
                  \> ConceptNode "Robert" noTv )

li :: Atom ListT
li = ListLink |> ConceptNode   "SomeConcept" (stv 1 1)
              |> PredicateNode "SomePredicate1" noTv
              |> PredicateNode "SomePredicate2" noTv
              \> ListLink |> ConceptNode   "Concept1" (stv 1 1)
                          |> PredicateNode "Predicate1" noTv
                          \> PredicateNode "Predicate2" noTv

-- Type checking Ok.
ex1 :: Atom ExecutionT
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

-- Type checking Ok.
findAnimals1 :: Atom BindT
findAnimals1 = BindLink
                  (VariableNode "$var")
                  (InheritanceLink noTv
                      (VariableNode "$var")
                      (ConceptNode "animal" noTv)
                  )
                  (VariableNode "$var")

{- Type checking error.
findAnimals2 = BindLink
                  (GroundedSchemaNode "some-fun") -- This is not a Variable.
                  (ListLink |> ConceptNode "Arg1" (stv 1 1)
                            \> ConceptNode "Arg2" (stv 1 1) )
                  (ConceptNode "res" (stv 1 1))
-}

main :: IO ()
main = runOnNewAtomSpace $ do
         p <- get $ PredicateNode "Pred" noTv
         case p of
            Just (PredicateNode _ _) -> liftIO $ putStrLn "Predicate found."
            _                        -> liftIO $ putStrLn "No Predicate found."
         liftIO $ printAtom li
         insert li
         res <- get li
         () <- case res of
           Just (ListLink (x:_)) -> case x of
               Gen (ConceptNode c _)    -> liftIO $ putStrLn "First is Concept"
               Gen (PredicateNode p _ ) -> liftIO $ putStrLn "First is Predicate"
               _                        -> liftIO $ putStrLn "First is other type"
         insert e
         remove e
         return ()

