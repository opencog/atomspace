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
module OpenCog.Test.TemplateData where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import OpenCog.AtomSpace hiding (ListT)
import qualified Data.Map as M
import Data.List (isPrefixOf,delete)


generatedTestData :: Q [Dec]
generatedTestData = do
    (TyConI a) <- reify (mkName "Atom")
    let exps = getCons a
        body = NormalB $ ListE $ map (\x -> AppE (ConE $ mkName "Gen") x) exps
        typd = SigD (mkName "testData") (AppT ListT (ConT $ mkName "AtomGen"))
        fund = FunD (mkName "testData") [(Clause [] body [])]
    return [typd,fund]

getCons :: Dec -> [Exp]
getCons d = case d of
    (DataD _ _ _ cs _) -> M.elems $ getCons' sorted M.empty
        where sorted = sortDeps $ map getDependency cs


sortDeps lst =
    let res = sortDeps' lst []
    in case res of
        ([],u) -> map (\(c,n,_) -> (n,c)) u
        (r,[]) -> r
        (r,u) -> r ++ sortDeps u

sortDeps' [] ls            = ([],ls)
sortDeps' ((c,n,[]):xs) ls = ((n,c) : a,b)
    where (a,b) = sortDeps' (deleteDep n xs) ls
sortDeps' (x:xs) ls        = sortDeps' xs (x:ls)

deleteDep x [] = []
deleteDep x ((c,e,ls):rst) = (c,e,delete x ls): deleteDep x rst

getDependency :: Con -> (Con,String,[String])
getDependency con@(ForallC _ ctx (NormalC n _)) = (con,name,dep)
    where dep = dups $ delete name $ map ctxDep ctx
          name = nameToString n

dups []     = []
dups (x:xs) = x : delete x (dups xs)

nameToString :: Name -> String
nameToString n = replace "" "T" (replace "" "Link" (replace "" "Node" (show $ simpleName n)))

replace :: String -> String -> String -> String
replace a b [] = []
replace a b c | b `isPrefixOf` c = a ++ drop (length b) c
replace a b (x:xs) = x:replace a b xs

ctxDep (AppT _ (ConT t)) = nameToString t
ctxDep x = error $ "ctxDep: " ++ show x

getCons' :: [(String,Con)] -> M.Map String Exp -> M.Map String Exp
getCons' [] map        = map
getCons' ((n,c):r) map = getCons' r $ M.insert n (getCon c map) map

getCon :: Con -> M.Map String Exp -> Exp
getCon (ForallC tvb ctx (NormalC n sts)) semap = concatExps (ConE $ simpleName n) es
    where ts     = map snd sts
          es     = map (typeToExp ff semap) ts
          cs     = map ctxString ctx
          ff inp = foldr (\x f -> case f of
                                    Left s -> case (x s) of
                                        (Just r) ->  Right r
                                        _ -> Left s
                                    Right r -> Right r) (Left inp) cs

ctxString :: Type -> String -> Maybe Name
ctxString (AppT (AppT _ (VarT name)) (ConT t)) =
    (\x -> if x == show name then Just t else Nothing)
ctxString x = error $ "ctxString: " ++ show x

atomname = "OpenCog.AtomSpace.Types.AtomName"
truthval = "OpenCog.AtomSpace.Types.TruthVal"
double   = "GHC.Types.Double"

typeToExp _  _ (ConT name)
    | (show name) == atomname = LitE $ StringL "testName"
    | (show name) == truthval = VarE $ mkName "noTv"
    | (show name) == double  = LitE $ IntegerL 0
    | otherwise = error $ show name
typeToExp _  _ (AppT ListT _) = ListE [ AppE (ConE $ mkName "Gen") (AppE (ConE $ mkName "VariableNode") (LitE $ StringL "VarNode"))]
typeToExp c semap (AppT _ (VarT x)) =
    let Right tpe = (c (show x))
        tn    = nameToString tpe
    in case (M.lookup tn semap) of
        Just x -> x
        Nothing -> AppE (ConE $ mkName "VariableNode") (LitE $ StringL "VarNode")

--AppE (ConE $ mkName "VariableNode") (LitE $ StringL "varNode")

concatExps :: Exp -> [Exp] -> Exp
concatExps n []     = error "concatExps expects non-empty list"
concatExps n [x]    = AppE n x
concatExps n (x:xs) = concatExps (AppE n x) xs

simpleName :: Name -> Name
simpleName nm =
   let s = nameBase nm
   in case dropWhile (/=':') s of
        []          -> mkName s
        [_]         -> mkName s
        _:t         -> mkName t
