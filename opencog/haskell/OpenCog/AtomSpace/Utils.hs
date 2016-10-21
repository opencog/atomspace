{-# LANGUAGE DataKinds      #-}
-- GSoC 2015 - Haskell bindings for OpenCog.

-- | This Module offers useful functions for working on an AtomSpace.
module OpenCog.AtomSpace.Utils (
      showAtom
    , printAtom
    , atomMap
    , atomMapM
    , atomFold
    , atomElem
    , nodeName
    , atomType
    , atomGetAllNodes
    ) where

import OpenCog.AtomSpace.Types      (Atom(..),TruthVal(..))
import OpenCog.AtomSpace.Internal   (fromTVRaw,toTVRaw)
import OpenCog.AtomSpace.Api        (get,insert)
import OpenCog.AtomSpace.Types
import OpenCog.AtomSpace.Env         (AtomSpace)
import Data.Functor                 ((<$>))
import Data.Typeable                (Typeable)

-- | 'showTV' shows a truth value in opencog notation.
showTV :: TruthVal -> String
showTV (SimpleTV a b     ) = "(stv "++show a++" "++show b++")"
showTV (CountTV a b c    ) = "(ctv "++show a++" "++show b++" "++show c++")"
showTV (IndefTV a b c d e) = "(itv "++show a++" "++show b++" "
                                    ++show c++" "++show d++" "
                                    ++show e++")"
showTV (FuzzyTV a b      ) = "(ftv "++show a++" "++show b++")"
showTV (ProbTV a b c     ) = "(ptv "++show a++" "++show b++" "++show c++")"

showTV' :: Maybe TruthVal -> String
showTV' (Just tv) = showTV tv
showTV' Nothing   = ""

-- | 'showAtom' shows an atom in opencog notation (indented notation).
showAtom :: Atom -> String
showAtom at = concatWNewline $ list 0 at
  where
    list :: Int -> Atom -> [String]
    list lv at = case at of
      Link atype lraw  tv -> let showtv = showTV tv
                              in [tab lv $ concatWSpaces [atype,showtv]]
                                 ++ concat (map (list (lv+1)) lraw)
      Node atype aname tv -> let showtv = showTV tv
                              in [tab lv $ concatWSpaces [atype,showtv
                                                         ,"\""++aname++"\""]]

    concatWNewline :: [String] -> String
    concatWNewline []     = []
    concatWNewline (x:xs) = foldr1 (\a b -> a++"\n"++b) (x:xs)

    concatWSpaces :: [String] -> String
    concatWSpaces []     = []
    concatWSpaces (x:xs) = foldr1 (\a b -> if a /= ""
                                            then a++" "++b
                                            else b) (x:xs)

    tab :: Int -> String -> String
    tab 0 s  = s
    tab lv s = "  "++ tab (lv-1) s

-- | 'printAtom' prints the given atom on stdout.
printAtom :: Atom -> IO ()
printAtom at = putStrLn $ showAtom at


-- Atoms can build a tree structure
-- the following functions make it easier to
-- apply functions to all Atoms in such a tree

atomMap :: (Atom -> Atom) -> Atom -> Atom
atomMap f (Link t ls tv) = f $ Link t (map (atomMap f) ls) tv
atomMap f n@(Node _ _ _) = f n

atomMapM :: Monad m => (Atom -> m Atom) -> Atom -> m Atom
atomMapM f (Link t ls tv) = do
    nls <- (mapM (atomMapM f) ls)
    f $ Link t nls tv
atomMapM f n@(Node _ _ _) = f n

atomFold :: (a -> Atom -> a) -> a -> Atom -> a
atomFold f v a@(Link t ls tv) = f (foldl (atomFold f) v ls) a
atomFold f v a@(Node _ _ _)   = f v a

atomElem :: Atom -> Atom -> Bool
atomElem n@(Node _ _ _) a@(Node _ _ _) = n == a
atomElem n@(Node _ _ _) a@(Link _ _ _) =
    atomFold (\b a -> a == n || b) False a
atomElem n@(Link _ _ _) a@(Node _ _ _) = False
atomElem n@(Link _ _ _) a@(Link _ _ _) =
    atomFold (\ b a -> a == n || b) False a

atomGetAllNodes :: Atom -> [Atom]
atomGetAllNodes n@(Node _ _ _) = [n]
atomGetAllNodes (Link _ ls _)  = concatMap atomGetAllNodes ls

nodeName :: Atom -> String
nodeName (Node _ n _) = n
nodeName (Link _ _ _) = error "nodeName expects a Node."

atomType :: Atom -> String
atomType (Node t _ _) = t
atomType (Link t _ _) = t

