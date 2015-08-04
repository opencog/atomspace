{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module OpenCog.AtomSpace.Template (
  declareAtomType
, declareAtomFilters
, atomHierarchyFile
) where

import Language.Haskell.TH.Quote (QuasiQuoter(..),dataToExpQ,quoteFile)
import Language.Haskell.TH
import Data.List                 (isSuffixOf,sortBy,(\\),groupBy)
import Data.Char                 (toUpper,toLower)
import Data.Data                 (Data,Typeable)

data AT = NOD String
        | LNK String
    deriving (Typeable,Data,Eq,Ord,Show)

declareAtomType :: [(AT,[AT])] -> Q [Dec]
declareAtomType ll = do
    a <- newName "a"
    let
      atomType      = mkName "AtomType"
      functionName1 = mkName "toAtomTypeRaw"
      functionName2 = mkName "fromAtomTypeRaw"
      typeFamName1  = mkName "Up"
      typeFamName2  = mkName "Down"
      constrNames   = map (\(nod,parents) ->
                                (mkName (toTypeName nod)
                                ,toRawName nod
                                ,map (mkName . toTypeName) parents)) ll
      revTree       = reverseTree ll
      constrNamesRev= map (\(nod,children) ->
                                (mkName (toTypeName nod)
                                ,map (mkName . toTypeName) children)) revTree
      constr      = map (\(x,_,_) -> NormalC x []) constrNames
      typeDef     = DataD [] atomType [] constr [''Eq, ''Show]
      funDef1     = FunD functionName1 (map createClause1 constrNames)
      funDef2     = FunD functionName2 (map createClause2 constrNames)
      typeFamDef1 = ClosedTypeFamilyD typeFamName1 [PlainTV a]
                    (Just (AppT ListT (ConT atomType)))
                    (map createClause3 constrNames)
      typeFamDef2 = ClosedTypeFamilyD typeFamName2 [PlainTV a]
                    (Just (AppT ListT (ConT atomType)))
                    (map createClause4 constrNamesRev)

      createClause1 (n,s,_)  = Clause [ConP n []]
                                 (NormalB (LitE (StringL s)))
                                 []

      createClause2 (n,s,_)  = Clause [LitP (StringL s)]
                                 (NormalB (AppE (ConE $ mkName "Just") (ConE n)))
                                 []

      createClause3 (n,_,p)  = TySynEqn [PromotedT n] (genlist p)

      createClause4 (n,p)    = TySynEqn [PromotedT n] (genlist p)

     in return [typeDef,funDef1,funDef2,typeFamDef1,typeFamDef2]
  where
    genlist []     = PromotedNilT
    genlist (x:xs) = AppT (AppT PromotedConsT (PromotedT x)) (genlist xs)

declareAtomFilters :: [(AT,[AT])] -> Q [Dec]
declareAtomFilters ll = do
    let
      className     = mkName "FilterIsChild"
      classFnName   = mkName "filtIsChild"
      constrNames   = map (\(nod,_) -> mkName (toTypeName nod)) ll
      classDef      = (map createInstance constrNames)

      createInstance n = InstanceD []
          (AppT (ConT className) (PromotedT n))
          [ValD (VarP classFnName) (NormalB (AppE (VarE $ mkName "filtChild")
            (SigE
              (ConE $ mkName "Proxy")
              (AppT (ConT $ mkName "Proxy")
                    (AppT (ConT $ mkName "Children")
                          (PromotedT n)))))) []
          ]
     in return classDef

atomHierarchyFile :: QuasiQuoter
atomHierarchyFile = quoteFile atomHierarchy

atomHierarchy :: QuasiQuoter
atomHierarchy = QuasiQuoter
    { quoteExp = dataToExpQ (\x -> Nothing) . parser
    }

parser :: String -> [(AT,[AT])]
parser = map toATTuple
       . map toCamelTuple
       . concat
       . map parseLine
       . map removeComm
       . lines

removeComm :: String -> String
removeComm ('/':'/':_) = []
removeComm ('"':_) = []
removeComm (x:xs) = x : removeComm xs
removeComm [] = []

parseLine :: String -> [(String,[String])]
parseLine s = case words (map repl s) of
    aname:[]      -> [(aname,[])]
    aname:"<-":xs -> [(aname,xs)]
    _             -> []
  where
    repl ',' = ' '
    repl  x  = x

toCamelCase :: String -> String
toCamelCase = concat
            . map capital
            . words
            . map repl
  where
      repl '_' = ' '
      repl  x  = x
      capital (x:xs) = toUpper x : map toLower xs
      capital []     = []

toCamelTuple :: (String,[String]) -> (String,[String])
toCamelTuple (x,xs) = (toCamelCase x,map toCamelCase xs)

toAT :: String -> AT
toAT "Notype" = NOD "Notype"
toAT "Atom"   = NOD "Atom"
toAT "Node"   = NOD "Node"
toAT "Link"   = LNK "Link"
toAT xs | isSuffixOf "Node" xs = NOD $ take (length xs - 4) xs
        | isSuffixOf "Link" xs = LNK $ take (length xs - 4) xs
        | otherwise            = LNK xs

toATTuple :: (String,[String]) -> (AT,[AT])
toATTuple (x,xs) = (toAT x,map toAT xs)

toTypeName :: AT -> String
toTypeName (NOD s) = s ++ "T"
toTypeName (LNK s) = s ++ "T"

toRawName :: AT -> String
toRawName (NOD "Notype") = "Notype"
toRawName (NOD "Atom"  ) = "Atom"
toRawName (NOD "Node"  ) = "Node"
toRawName (LNK "Link"  ) = "Link"
toRawName (NOD n       ) = n ++ "Node"
toRawName (LNK l       ) = l ++ "Link"

reverseTree :: [(AT,[AT])] -> [(AT,[AT])]
reverseTree t = let rt = reverseTree' t
                 in rt ++ map (\x -> (x,[])) (map fst t \\ map fst rt)

reverseTree' :: [(AT,[AT])] -> [(AT,[AT])]
reverseTree' = concat
             . map aux2
             . groupBy (\x y -> fst x == fst y)
             . sortBy (\a b -> compare (fst a) (fst b))
             . concat
             . map aux
  where
    aux :: (AT,[AT]) -> [(AT,AT)]
    aux (x,xs) = map (\y -> (y,x)) xs
    aux2 :: [(AT,AT)] -> [(AT,[AT])]
    aux2 []         = []
    aux2 ((x,y):ys) = [(x,y : map snd ys)]

