{-# LANGUAGE DataKinds      #-}
-- GSoC 2015 - Haskell bindings for OpenCog.

-- | This Module offers useful functions for working on an AtomSpace.
module OpenCog.AtomSpace.Utils (
      showAtom
    , printAtom
    , genInsert
    , genGet
    ) where

import OpenCog.AtomSpace.Types      (Atom(..),TruthVal(..))
import OpenCog.AtomSpace.Internal   (fromTVRaw,toRaw,AtomRaw(..))
import OpenCog.AtomSpace.Api        (get,insert)
import OpenCog.AtomSpace.Types
import OpenCog.AtomSpace.AtomType    (AtomType(..))
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
showAtom :: Typeable a => Atom a -> String
showAtom at = concatWNewline $ list 0 $ toRaw at
  where
    list :: Int -> AtomRaw -> [String]
    list lv at = case at of
      Link atype lraw  tv -> let showtv = showTV' $ fromTVRaw <$> tv
                              in [tab lv $ concatWSpaces [atype,showtv]]
                                 ++ concat (map (list (lv+1)) lraw)
      Node atype aname tv -> let showtv = showTV' $ fromTVRaw <$> tv
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
printAtom :: Typeable a => Atom a -> IO ()
printAtom at = putStrLn $ showAtom at

genInsert :: Gen a -> AtomSpace ()
genInsert a = appGen insert a

genGet :: Gen AtomT -> AtomSpace (Maybe (Gen AtomT))
genGet (Gen a) = do
    res <- get a
    case res of
        Just x ->  return $ Just $ Gen x
        Nothing -> return $ Nothing
