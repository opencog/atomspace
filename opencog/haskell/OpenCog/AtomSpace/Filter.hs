-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}

-- | This Module defines atom filters based on their hierarchy.
module OpenCog.AtomSpace.Filter (
    Gen(..)
  , appGen
  , FilterIsChild(..)
  ) where

import OpenCog.AtomSpace.Inheritance    (type (<~),Children)
import OpenCog.AtomSpace.AtomType       (AtomType(..))
import OpenCog.AtomSpace.Types          (Atom(..))
import Data.Proxy                       (Proxy(..))
import Data.Typeable                    (cast,Typeable)

-- | 'Gen' groups all the atoms that are children of the atom type a.
data Gen a where
    Gen :: (b <~ a) => Atom b -> Gen a

-- | 'appGen' evaluates a given function with the atom type instance
-- wrapped inside the 'Gen' type.
appGen :: (forall b. (b <~ a) => Atom b -> c) -> Gen a -> c
appGen f (Gen at) = f at

-- | 'Filter' class defines a filter on the list 'b' of atom types.
class Filter a (b::[AtomType]) where
    filtChild :: Typeable c => Proxy b -> Atom c -> Maybe (Gen a)

instance Filter e '[] where
    filtChild _ _ = Nothing

instance (Typeable x,x <~ e,Filter e xs) => Filter e (x ': xs) where
    filtChild _ a = case cast a :: Maybe (Atom x) of
        Just res -> return $ Gen res
        Nothing  -> filtChild (Proxy :: Proxy xs) a

-- | 'FilterIsChild' class defines a filter on the descendants of atom type 'a'.
class FilterIsChild a where
    filtIsChild :: (b <~ AtomT) => Atom b -> Maybe (Gen a)

instance FilterIsChild AtomT where
    filtIsChild = filtChild (Proxy :: Proxy (Children AtomT))
instance FilterIsChild NodeT where
    filtIsChild = filtChild (Proxy :: Proxy (Children NodeT))
instance FilterIsChild LinkT where
    filtIsChild = filtChild (Proxy :: Proxy (Children LinkT))
instance FilterIsChild PredicateT where
    filtIsChild = filtChild (Proxy :: Proxy (Children PredicateT))
instance FilterIsChild ConceptT where
    filtIsChild = filtChild (Proxy :: Proxy (Children ConceptT))
instance FilterIsChild SchemaT where
    filtIsChild = filtChild (Proxy :: Proxy (Children SchemaT))
instance FilterIsChild GroundedSchemaT where
    filtIsChild = filtChild (Proxy :: Proxy (Children GroundedSchemaT))
instance FilterIsChild NumberT where
    filtIsChild = filtChild (Proxy :: Proxy (Children NumberT))
instance FilterIsChild AndT where
    filtIsChild = filtChild (Proxy :: Proxy (Children AndT))
instance FilterIsChild OrT where
    filtIsChild = filtChild (Proxy :: Proxy (Children OrT))
instance FilterIsChild ImplicationT where
    filtIsChild = filtChild (Proxy :: Proxy (Children ImplicationT))
instance FilterIsChild EquivalenceT where
    filtIsChild = filtChild (Proxy :: Proxy (Children EquivalenceT))
instance FilterIsChild EvaluationT where
    filtIsChild = filtChild (Proxy :: Proxy (Children EvaluationT))
instance FilterIsChild InheritanceT where
    filtIsChild = filtChild (Proxy :: Proxy (Children InheritanceT))
instance FilterIsChild SimilarityT where
    filtIsChild = filtChild (Proxy :: Proxy (Children SimilarityT))
instance FilterIsChild MemberT where
    filtIsChild = filtChild (Proxy :: Proxy (Children MemberT))
instance FilterIsChild SatisfyingSetT where
    filtIsChild = filtChild (Proxy :: Proxy (Children SatisfyingSetT))
instance FilterIsChild ListT where
    filtIsChild = filtChild (Proxy :: Proxy (Children ListT))
instance FilterIsChild ExecutionT where
    filtIsChild = filtChild (Proxy :: Proxy (Children ExecutionT))
