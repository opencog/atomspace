
module OpenCog.AtomSpace.Types(
    NodeType (..)
  , LinkType (..)
  , TruthValue (..)
  , CogAtom (..)
  , CogLink (..)
  , CogNode (..)
  , AtomSpace (..)
  ) where

import Data.Default
import Foreign

data NodeType = NodeType1 | ConceptNode
    deriving (Enum,Show)

data LinkType = LinkType1 | LinkType2
    deriving (Enum,Show)

type TruthValue = (Double,Double)

data CogAtom = CLink CogLink | CNode CogNode 
    deriving Show

data CogLink = CogLink {
    link_type  :: LinkType
,   link_name  :: String
,   link_tv    :: TruthValue
}   deriving Show

instance Default CogLink where
    def = CogLink LinkType1 "" (1,0)

data CogNode = CogNode{
    node_type  :: NodeType
,   node_name  :: String
,   node_tv    :: TruthValue
}   deriving Show

instance Default CogNode where
    def = CogNode ConceptNode "" (0,0)

newtype AtomSpace = AtomSpace (Ptr AtomSpace)

