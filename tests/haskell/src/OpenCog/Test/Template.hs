{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE DataKinds              #-}
--{-# LANGUAGE TypeSynonymInstances   #-}
module OpenCog.Test.Template where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import OpenCog.AtomSpace (appGen,Gen(..),Atom,type (<~),TruthVal,AtomType(AtomT,NotT))
import Data.Typeable
import Debug.Trace
import Data.Char
import Data.List
import Control.Monad
import Data.Foldable
import Data.Maybe
import Prelude hiding (fst)

type TVal = Maybe TruthVal

errorS s = error $ show s

fst (a,b,c) = a

declareInstanceSerialAtom :: FilePath -> Q [Dec]
declareInstanceSerialAtom file = do
    fullatomtree <- runIO $ parsefile file
    (TyConI a)   <- reify (mkName "Atom")
    let constructors = getCons a
        conNames     = map (nameBase.fst) constructors
        [atomtree]   = pruneAtomTree conNames [fullatomtree]
        conTree      = toConstructors atomtree constructors
    customFoldAtomTree conTree

uncurry0 = id
uncurry1 = uncurry
uncurry2 f (a,b,c) = (uncurry . uncurry) f ((a,b),c)
myappGen2a :: (forall b. (Typeable bb, b <~ bb) =>
              a -> Atom b -> r) -> (a, Gen bb) -> r
myappGen2a f (a,Gen b) = f a b
myappGen2b :: (forall a b. (Typeable a, Typeable b,
              a <~ aa,b <~ bb) => (Atom a -> Atom b -> r)) ->
              (Gen aa, Gen bb) -> r
myappGen2b f (Gen a,Gen b) = f a b
myappGen3a :: (forall b c. (Typeable b, Typeable c,
              b <~ bb,c <~ cc) => (a -> Atom b -> Atom c -> r)) ->
              (a,Gen bb, Gen cc) -> r
myappGen3a f (a,Gen b,Gen c) = f a b c
myappGen3b :: (forall a b c. (Typeable a,Typeable b, Typeable c,
              a <~ aa,b <~ bb,c <~ cc) =>
              (Atom a -> Atom b -> Atom c -> r)) ->
              (Gen aa,Gen bb, Gen cc) -> r
myappGen3b f (Gen a,Gen b,Gen c) = f a b c

customFoldAtomTree :: Atomtree (String,Maybe (String -> String -> Exp)) -> Q [Dec]
customFoldAtomTree tree = do
    let series           = VarP $ mkName "series"
        dec c            = [ValD series (NormalB c) []]
        createInstance a = [d| instance Monad m => Serial m (Gen $(return $ typename a)) where|]
    case tree of
        (Leaf (a,Just c)) -> do
            [InstanceD x y _] <- createInstance a
            return [InstanceD x y (dec $ c a a)]
        (Node (a,c) ls) -> do
            [InstanceD x y _] <- createInstance a
            sub <- mapM customFoldAtomTree ls
            let subinst     = concat sub
                exp         = concantExpWith (VarE $ mkName "\\/") $ subcons ls
                subcons lst = foldr ff [] lst
                ff x b      = case x of
                    (Leaf (n,Just cc))     -> cc n a : b
                    (Node (n,Just cc) as)  -> subcons as ++ cc n a : b
                    (Node (n,Nothing) as)  -> subcons as ++ b
                    _                      -> b
            this <- case c of
                Just cons -> do
                    conss <- [e| $(return $ cons a a) \/ $(return exp) |]
                    return [InstanceD x y (dec conss)]
                Nothing -> return [InstanceD x y (dec exp)]
            return (this++subinst)

concantExpWith :: Exp -> [Exp] -> Exp
concantExpWith o [x]      = x
concantExpWith o [x,y]    = UInfixE x o y
concantExpWith o (x:xs)   = UInfixE x o (concantExpWith o xs)

typename s = PromotedT (mkName (consToType s))
    where consToType "Node"                    = "NodeT"
          consToType "Link"                    = "LinkT"
          consToType "Atom"                    = "AtomT"
          consToType "ForallLink"              = "ForAllT"
          consToType a | "Link" `isSuffixOf` a = replace "T" "Link" a
                       | "Node" `isSuffixOf` a = replace "T" "Node" a
                       | otherwise             = a++"T"

toConstructors :: AtomTree -> [(Name,Int,String -> Type)]
                  -> Atomtree (String,Maybe (String -> String -> Exp))
toConstructors tree constructors = fmap mkCons tree
    where mkCons s = case getConsName s of
            Just (i,t) -> (s,Just $ createConstructorCreator i t)
            Nothing    -> (s,Nothing)

          getConsName s = case filter (\(a,_,_) -> nameBase a == s) constructors of
            [(_,i,t)] -> Just (i,t)
            _         -> Nothing
          createConstructorCreator i t a b = case (i,isNode a) of
              (i,True)  -> myapp (mydot (uncry (i-1) (conE a)))
              (i,False) -> myapp (mydot $ function (i-1) a)
              where expType = t b
                    varE n  = VarE $ mkName n
                    conE n  = ConE $ mkName n
                    cons i  = varE ("cons"++show i)
                    uncry n = AppE (varE $ "uncurry"++show n)
                    mydot   = UInfixE (conE "Gen") (varE ".")
                    myapp x = AppE (cons 1) (SigE x expType)

                    (ForallT _ _ (AppT ta (AppT _ _))) = expType
                    subtype      = AppT ta (AppT (ConT $ mkName "Atom") (typename a))

                    function i a = case toAcceptGen $ expType of
                        (str,True) -> SigE (AppE (varE str) (conE a)) subtype
                        (_  ,False)-> uncry i (conE a)
                    isNode a     = "Node" `isSuffixOf` a

                    toAcceptGen (ForallT _ _ (AppT (AppT ArrowT m) _)) = sub m
                    toAcceptGen a = errorS a

--Figure out which helper function applies to to the structure of this Atom
sub (AppT (AppT (TupleT 2) (ConT _  )) (AppT ListT _))             = ("",False)
sub (AppT (AppT (TupleT 2) (ConT _  )) (AppT _ _))                 = ("myappGen2a",True)
sub (AppT (AppT (TupleT 2) (AppT _ _)) (AppT _ _))                 = ("myappGen2b",True)
sub (AppT (AppT (AppT (TupleT 3) (ConT _  ))(AppT _ _))(AppT _ _)) = ("myappGen3a",True)
sub (AppT (AppT (AppT (TupleT 3) (AppT _ _))(AppT _ _))(AppT _ _)) = ("myappGen3b",True)
sub (ConT _)                                                       = ("",False)
sub (AppT ListT _)                                                 = ("",False)
sub (AppT _ _)                                                     = ("appGen",True)

replace :: String -> String -> String -> String
replace a b [] = []
replace a b c | b `isPrefixOf` c = a ++ drop (length b) c
replace a b (x:xs) = x:replace a b xs

data Atomtree a = Leaf a | Node a [Atomtree a]
type AtomTree = Atomtree String

instance (Show a) => (Show (Atomtree a)) where
    show (Leaf a) = "(Leaf " ++ show a ++ ")"
    show (Node a ls) = "(Node " ++ show a ++ "[" ++ concatMap show ls ++ "])"
deriving instance (Eq a) => (Eq (Atomtree a))
deriving instance (Foldable Atomtree)
deriving instance (Functor Atomtree)
deriving instance (Traversable Atomtree)

addAtom :: (String,String) -> AtomTree -> AtomTree
addAtom (c,p) (Leaf x)   | p == x = Node p [Leaf c]
addAtom (c,p) (Node x ls)| p == x = Node p $ Leaf c : ls
addAtom _     (Leaf x)            = Leaf x
addAtom cp    (Node x ls)         = Node x $ map (addAtom cp) ls

parsefile :: FilePath -> IO AtomTree
parsefile file = do
    cont <- readFile file
    let ff a = not ("//" `isPrefixOf` a) && a /= "" && not ("PATTERN_LINK" `isInfixOf` a)
        rels = filter ff $ lines cont
    return $ relsToAtomTree (drop 2 rels) (Leaf "Atom")

relsToAtomTree :: [String] -> AtomTree -> AtomTree
relsToAtomTree [] tree = tree
relsToAtomTree (x:xs) tree = relsToAtomTree xs (addAtom (a,b) tree)
    where [a,_,b] = map toCamelCase $ take 3 $ words x

--Remove All Leafes/Nodes that have no Constructors
pruneAtomTree :: [String] -> [AtomTree] -> [AtomTree]
pruneAtomTree  _ [] = []
pruneAtomTree a (Node b sl:ys) = (let r = pruneAtomTree a sl
                                  in if null r
                                     then [Leaf b | [b] `isInfixOf` a]
                                     else [Node b r])
                                 ++ pruneAtomTree a ys
pruneAtomTree a (Leaf b:ys) = [Leaf b | [b] `isInfixOf` a ] ++ pruneAtomTree a ys

toCamelCase :: String -> String
toCamelCase = concatMap capital
            . words
            . map repl
  where
      repl '_' = ' '
      repl  x  = x
      capital (x:xs) = toUpper x : map toLower xs
      capital []     = []

getCons :: Dec -> [(Name,Int,String -> Type)]
getCons d = case d of
    d@(DataD _ _ dtvb c _) -> map (getCon dtvb) c

getCon dtvb c = let conA (NormalC c xs)      = (simpleName c, length xs)
                    conA (RecC c xs)         = (simpleName c, length xs)
                    conA (InfixC _ c _)      = (simpleName c, 2)
                    conA (ForallC _ _ c)     = conA c
                    conToType (NormalC c [xs]) = snd xs
                    conToType (NormalC c xs) = foldr (flip AppT) (TupleT $ length xs) $ reverse $ map snd xs
                    appt t n = AppT (AppT ArrowT t) (AppT (ConT $ mkName "Gen") n)
                in case c of
    (ForallC tvb (t:ctx) con) -> let (n,i) = conA con
                                     (AppT a@(AppT _ x) _) = t
                                     newt y = AppT a (typename y)
                                     typeNs = findTypes ctx
                                     types  = rVWT (conToType con) typeNs
                                     func y = ForallT (dtvb++tvb)
                                                      [newt y]
                                                      (appt types x)
                                 in (n,i,func)

findTypes [] = []
findTypes (AppT (AppT _ (VarT vname)) (ConT cname):xs) = (vname,cname):findTypes xs
findTypes (_:xs)                    = findTypes xs

--replaceVarWithType
rVWT (AppT a b) names = AppT (rVWT a names)(rVWT b names)
rVWT (ConT a)  _ | nameBase a == "Atom" = ConT $ mkName "Gen"
rVWT (VarT a) ((vname,cname):xs) | a == vname  = ConT cname
                                 | null xs     = VarT a
                                 | otherwise   = rVWT (VarT a) xs
rVWT a _ = a

simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        [_]         -> mkName s
        _:t         -> mkName t
