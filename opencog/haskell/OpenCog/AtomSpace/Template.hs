-- GSoC 2015 - Haskell bindings for OpenCog.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | This Module defines the main util functions to use Template Haskell
-- in order to reduce as much boilerplate code as possible.
module OpenCog.AtomSpace.Template (
    declareAtomType
  , declareAtomFilters
  ) where

import Language.Haskell.TH.Quote        (QuasiQuoter(..),dataToExpQ,quoteFile)
import Language.Haskell.TH.Syntax       (addDependentFile)
import Language.Haskell.TH
import Data.List                        (isSuffixOf,sortBy,(\\),groupBy)
import Data.Char                        (toUpper,toLower)
import Data.Data                        (Data,Typeable)
import Data.Functor                     ((<$>))
import Data.Map.Strict                  (Map,mapKeys,toList,fromList,insert,
                                         empty,(!),keys,member)
import qualified Data.Map.Strict as M   (map)
import qualified Data.Set        as S   (Set,(\\),fromList,toList)
import Control.Monad.State              (State,modify,get,execState)
import System.Directory                 (getCurrentDirectory,doesFileExist)
import System.FilePath                  ((</>),takeDirectory)

-- | Simple Atom type.
type AT = String

-- | Template function to define AtomType and some util functions over it.
--   It takes as argument a relative path to the file "atom_types.script".
declareAtomType :: FilePath -> FilePath -> Q [Dec]
declareAtomType file1 file2 = do
    file <- chooseFile file1 file2
    atomMap <- parseFile file
    a <- newName "a"
    b <- newName "b"
    let
      atomType      = mkName "AtomType"
      functionName1 = mkName "toAtomTypeRaw"
      functionName2 = mkName "fromAtomTypeRaw"
      typeFamName1  = mkName "Is"
      typeFamName2  = mkName "Up"

      constrNames   = map (\(nod,ancestors) ->
                             (mkName (toTypeName nod)
                             ,nod
                             ,map (mkName . toTypeName) ancestors)) $
                               getFullAncestors atomMap
      constr        = map (\(x,_,_) -> NormalC x []) constrNames
      typeDef       = DataD [] atomType [] constr [''Eq, ''Show, ''Typeable, ''Read]
      funDef1       = FunD functionName1 (map createClause1 constrNames)
      funDef2       = FunD functionName2 (map createClause2 constrNames)

      typeFamDef1 = ClosedTypeFamilyD typeFamName1
                        [ KindedTV a (ConT atomType)
                        , KindedTV b (ConT atomType) ]
                        (Just (ConT $ mkName "Bool"))
                        (concat $ map createClause3 constrNames)

      typeFamDef2 = ClosedTypeFamilyD typeFamName2
                        [ PlainTV a ]
                        (Just (AppT ListT (ConT atomType)))
                        (map createClause4 constrNames)

      createClause1 (n,s,_) = Clause [ConP n []]
                                (NormalB (LitE (StringL s)))
                                []

      createClause2 (n,s,_) = Clause [LitP (StringL s)]
                                (NormalB (AppE (ConE $ mkName "Just") (ConE n)))
                                []

      createClause3 (n,_,p) = map (\x -> TySynEqn [PromotedT n,PromotedT x]
                                                  (PromotedT $ mkName "True"))
                                                  (n:p)

      createClause4 (n,_,p) = TySynEqn [PromotedT n] (genlist (n:p))

     in return [typeDef,funDef1,funDef2,typeFamDef1,typeFamDef2]
  where
    genlist []     = PromotedNilT
    genlist (x:xs) = AppT (AppT PromotedConsT (PromotedT x)) (genlist xs)

-- | Template function to declare Filter instances for each AtomType.
--   It takes as argument a relative path to the file "atom_types.script".
declareAtomFilters :: FilePath -> FilePath-> Q [Dec]
declareAtomFilters file1 file2 = do
    file <- chooseFile file1 file2
    atomMap <- parseFile file
    a <- newName "a"
    let
      className       = mkName "FilterIsChild"
      classFnName     = mkName "filtIsChild"
      phantTypeFnName = mkName "getPhantomType"
      castFnName      = mkName "cast"
      atomConst       = mkName "Atom"
      genConst        = mkName "Gen"
      nothing         = mkName "Nothing"

      revTree         = reverseTree atomMap
      completeRevTree = getFullDescendants revTree
      constrNames     = map (\(nod,children) ->
                                (mkName (toTypeName nod)
                                ,map (mkName . toTypeName) children)) completeRevTree
      classDef        = map createInstance constrNames

      createInstance (n,children) = InstanceD []
          (AppT (ConT className) (PromotedT n))
          [FunD classFnName
              [Clause [VarP a]
                      (NormalB (CaseE (AppE (VarE phantTypeFnName) (VarE a))
                                 ((map createClause (n:children))
                                  ++ [Match WildP (NormalB (ConE nothing)) []])
                               ))
                      []
              ]
          ]

      createClause n = Match (ConP n [])
                             (NormalB
                                (InfixE
                                   (Just (ConE genConst))
                                   (VarE $ mkName "<$>")
                                   (Just (SigE
                                          (AppE (VarE castFnName) (VarE a))
                                          (AppT (ConT $ mkName "Maybe")
                                          (AppT (ConT atomConst) (PromotedT n)))
                                         )
                                   )))
                             []

     in return classDef

-- | 'getAbsFromRelPath' obtains an absolute path from a relative path to
-- current file (where this template function is running).
getAbsFromRelPath :: FilePath -> Q FilePath
getAbsFromRelPath relative = do
    actual <- runIO getCurrentDirectory
    fileName <- loc_filename <$> location
    let path = actual </> fileName
        dir  = takeDirectory path
    return $ dir </> relative

chooseFile :: FilePath -> FilePath -> Q FilePath
chooseFile file1rel file2 = do
    file1 <- getAbsFromRelPath file1rel
    exist <- runIO $ doesFileExist file1
    return $ case exist of
        True -> file1
        False -> file2

-- | 'parseRelativeFile' takes a relative path to a file, and executes the
-- parser over it.
parseFile :: FilePath -> Q [(AT,[AT])]
parseFile file = do
    addDependentFile file
    cont <- runIO $ readFile file
    return $ parser cont

type NameMap = Map String String
type AtomMap = Map String [String]
type PState = State (NameMap,AtomMap)

onNameMap :: (NameMap -> NameMap) -> PState ()
onNameMap f = modify (\(s1,s2) -> (f s1,s2))

onAtomMap :: (AtomMap -> AtomMap) -> PState ()
onAtomMap f = modify (\(s1,s2) -> (s1,f s2))

-- | 'parser' reads the text of the atom_types.script file and generate a list
-- of tuples (Atom, Parents of that Atom).
parser :: String -> [(AT,[AT])]
parser s = ( toList
           . modifyVarNode
           . snd
           ) $ execState (withState s) (empty,empty)
  where
    withState :: String -> PState ()
    withState s = do
          onLines s
          mapToCamelCase
    onLines :: String -> PState ()
    onLines = mapM_ parseLine
            . map removeComm
            . lines
    mapToCamelCase :: PState ()
    mapToCamelCase = do
        (nameMap,_) <- get
        onAtomMap $ M.map $ map $ format nameMap
        onAtomMap $ mapKeys $ format nameMap
    format :: NameMap -> String -> String
    format dict s = if member s dict
                      then dict!s
                      else toCamelCase s
    -- This util function is used to modify the parents of VariableNode.
    -- We will set VariableNode's parent to all leaf nodes.
    -- So, VariableNode will inherit from every atom type (except it's children)
    -- , and we can place it everywhere without type constraints.
    varNode = "VariableNode"
    modifyVarNode :: Map AT [AT] -> Map AT [AT]
    modifyVarNode m = insert varNode parents m
        where lst = toList m
              children = getChildes lst varNode
              ff e = foldr (\a b -> a /= e && b) True (varNode:children)
              parents = filter (ff) $ getNodesLeaf lst

    getChildes :: [(AT,[AT])] -> AT -> [AT]
    getChildes m a = childs++(concatMap (getChildes m) childs)
        where childs = map fst $ filter ((elem a).snd) m

removeComm :: String -> String
removeComm ('/':'/':_) = []
removeComm (x:xs) = x : removeComm xs
removeComm [] = []

parseLine :: String -> PState ()
parseLine s = case words (map repl s) of
    aname:[]          -> onAtomMap $ insert aname []
    aname:"<-":(x:xs) -> case lookReName (x:xs) of
        Nothing   -> onAtomMap $ insert aname (x:xs)
        Just name -> do
            onNameMap $ insert aname name
            onAtomMap $ insert aname $ init (x:xs)
    _                 -> return ()
  where
    repl ',' = ' '
    repl  x  = x
    lookReName [] = Nothing
    lookReName xs = case (last xs,last $ last xs) of
        ('"':y:ys,'"') -> Just $ init (y:ys)
        _              -> Nothing

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

-- | 'toTypeName' given an atomtype generates a phantom type notation for it.
toTypeName :: AT -> String
toTypeName "Node" = "NodeT"
toTypeName "Link" = "LinkT"
toTypeName s
    | isSuffixOf "Node" s = take (length s - 4) s ++ "T"
    | isSuffixOf "Link" s = take (length s - 4) s ++ "T"
    | otherwise           = s ++ "T"

-- | 'reverseTree' reverses the information provided in the atom_types.script file.
-- From input: [(Atom, parent of Atom)]
-- It gets as output: [(Atom, children of Atom)]
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

-- | 'getFullDescendants' get all the descendants of each node.
-- (not only its next children).
-- From input: [(Atom, children of Atom)]
-- It gets as output: [(Atom, descendants of Atom)]
getFullDescendants :: [(AT,[AT])] -> [(AT,[AT])]
getFullDescendants l = let mAp = fromList l
                           desc k = S.toList $
                                    S.fromList $
                                    (mAp ! k) ++ (concat $ map desc (mAp ! k))
                        in map (\x -> (x,desc x)) (keys mAp)

-- | 'getFullAncestors' get all the ancestors of each node.
-- (not only its next parents).
-- From input: [(Atom, parents of Atom)]
-- It gets as output: [(Atom, ancestors of Atom)]
getFullAncestors :: [(AT,[AT])] -> [(AT,[AT])]
getFullAncestors = getFullDescendants

-- | 'getNodesLeaf' get all nodes that have no children.
-- From input: [(Atom, parents of Atom)]
-- It gets as output: [Atom]
getNodesLeaf :: [(AT,[AT])] -> [AT]
getNodesLeaf l = let s1 = S.fromList $ concat $ map snd l
                     s2 = S.fromList $ map fst l
                  in S.toList $ s2 S.\\ s1
