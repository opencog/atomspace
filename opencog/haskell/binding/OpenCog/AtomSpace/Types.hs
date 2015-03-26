
module OpenCog.AtomSpace.Types(
    NodeType (..)
  , LinkType (..)
  , TruthValue (..)
  , Atom (..)
  , Link (..)
  , Node (..)
  ) where

import Data.Default
import Foreign

-- I should add more options here, this is just an example.
data NodeType = NodeType1 | ConceptNode
    deriving (Enum,Show)

-- I should add more options here, this is just an example.
data LinkType = LinkType1 | LinkType2
    deriving (Enum,Show)

data TruthValue = SimpleTruthValue Double Double
                | CountTruthValue Double Double Double
                | IndefiniteTruthValue Double Double Double Double
    deriving Show

instance Default TruthValue where
    def = SimpleTruthValue 1 0

data Atom = CLink Link | CNode Node
    deriving Show

data Link = Link{
    linkType  :: LinkType
,   linkName  :: String
,   linkTv    :: TruthValue
,   linkAtoms :: [Atom]
}   deriving Show

instance Default Link where
    def = Link LinkType1 "" def []

data Node = Node{
    nodeType  :: NodeType
,   nodeName  :: String
,   nodeTv    :: TruthValue
}   deriving Show

instance Default Node where
    def = Node ConceptNode "" def

