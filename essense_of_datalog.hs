{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleDatalog where

import Data.Function (fix)
import Data.List (nub, intercalate, isSubsequenceOf)
import Data.Maybe (mapMaybe, fromMaybe, isNothing)

type Program = [ Rule ]

data Rule = Rule { _head :: Atom, _body :: [ Atom ] } deriving Show

data Atom = Atom { _predSym :: String, _terms :: [ Term ] } deriving (Eq, Show)

data Term = Var String | Sym String deriving (Eq, Show)

type KnowledgeBase = [ Atom ]

type Substitution = [ (Term, Term) ]

emptySubstitution :: Substitution
emptySubstitution = []

solve :: Program -> KnowledgeBase
solve rules =
  if all isRangeRestricted rules
    then fix step []
    else error "The input program is not range-restricted."
  where
  step :: (KnowledgeBase -> KnowledgeBase)
       -> (KnowledgeBase -> KnowledgeBase)
  step f currentKB | nextKB <- immediateConsequence rules currentKB =
    if nextKB == currentKB
      then currentKB
      else f nextKB

isRangeRestricted :: Rule -> Bool
isRangeRestricted Rule{..} =
  vars _head `isSubsetOf` concatMap vars _body
  where
  isSubsetOf as bs = all (`elem` bs) as
  vars Atom{..} = nub $ filter (\case {Var{} -> True; _ -> False}) _terms

immediateConsequence :: Program -> KnowledgeBase -> KnowledgeBase
immediateConsequence rules kb =
  nub . (kb <>) . concatMap (evalRule kb) $ rules

evalRule :: KnowledgeBase -> Rule -> KnowledgeBase
evalRule kb (Rule head body) = map (substitute head) (walk kb body)

walk :: KnowledgeBase -> [ Atom ] -> [ Substitution ]
walk kb = foldr (evalAtom kb) [ emptySubstitution ]

evalAtom :: KnowledgeBase -> Atom -> [ Substitution ] -> [ Substitution ]
evalAtom kb atom substitutions = do
  substitution <- substitutions
  let downToEarthAtom = substitute atom substitution
  extension <- mapMaybe (unify downToEarthAtom) kb
  return $ substitution <> extension

substitute :: Atom -> Substitution -> Atom
substitute atom substitution = atom { _terms = map go (_terms atom) }
  where
  go sym@Sym{} = sym
  go var@Var{} = fromMaybe var (var `lookup` substitution)

unify :: Atom -> Atom -> Maybe Substitution
unify (Atom predSym ts) (Atom predSym' ts')
  | predSym == predSym' = go $ zip ts ts'
  | otherwise           = Nothing
  where
  go :: [ (Term, Term) ] -> Maybe Substitution
  go []                           = Just emptySubstitution
  go ((s@Sym{}, s'@Sym{}) : rest) = if s == s' then go rest else Nothing
  go ((v@Var{}, s@Sym{})  : rest) = do
    incompleteSubstitution <- go rest
    case v `lookup` incompleteSubstitution of
      Just s' | s /= s'   -> Nothing
      _                   -> return $ (v,s) : incompleteSubstitution
  go ((_, Var{}) : _) = error "The second atom is assumed to be ground."

{-
- adviser("Andrew Rice",     "Mistral Contrastin").
- adviser("Dominic Orchard", "Andrew Rice").
- adviser("Andy Hopper",     "Andrew Rice").
- adviser("Alan Mycroft",    "Dominic Orchard").
- adviser("David Wheeler",   "Andy Hopper").
- adviser("Rod Burstall",    "Alan Mycroft").
- adviser("Robin Milner",    "Alan Mycroft").
-
- academicAncestor(X,Y) :- adviser(X,Y).
- academicAncestor(X,Z) :- adviser(X,Y), academicAncestor(Y,Z).
-
- ?- academicAncestor("Robin Milner", Intermediate),
-    academicAncestor(Intermediate, "Mistral Contrastin").
- ?- academicAncestor("Alan Turing", "Mistral Contrastin").
- ?- academicAncestor("David Wheeler", "Mistral Contrastin").
-}
ancestor :: Program
ancestor =
  -- Facts
  fmap (\terms -> Rule (Atom "adviser" terms) [])
    [ [ Sym "Andrew Rice",     Sym "Mistral Contrastin" ]
    , [ Sym "Dominic Orchard", Sym "Mistral Contrastin" ]
    , [ Sym "Andy Hopper",     Sym "Andrew Rice" ]
    , [ Sym "Alan Mycroft",    Sym "Dominic Orchard" ]
    , [ Sym "David Wheeler",   Sym "Andy Hopper" ]
    , [ Sym "Rod Burstall",    Sym "Alan Mycroft" ]
    , [ Sym "Robin Milner",    Sym "Alan Mycroft" ]
    ] <>
  -- Actual rules
  [ Rule (Atom "academicAncestor" [ Var "X", Var "Y" ])
      [ Atom "adviser" [ Var "X", Var "Y" ] ]
  , Rule (Atom "academicAncestor" [ Var "X", Var "Z" ])
      [ Atom "adviser"          [ Var "X", Var "Y" ]
      , Atom "academicAncestor" [ Var "Y", Var "Z" ] ]
  ] <>
  -- Queries
  [ Rule (Atom "query1" [ Var "Intermediate" ])
      (fmap (Atom "academicAncestor")
        [ [ Sym "Robin Milner", Var "Intermediate" ]
        , [ Var "Intermediate", Sym "Mistral Contrastin" ] ])
  , Rule (Atom "query2" [ ])
      [ Atom "academicAncestor"
          [ Sym "Alan Turing", Sym "Mistral Contrastin" ] ]
  , Rule (Atom "query3" [ ])
      [ Atom "academicAncestor"
          [ Sym "David Wheeler", Sym "Mistral Contrastin" ] ]
  ]

query :: String -> Program -> [ Substitution ]
query predSym pr =
  case queryVarsL of
    [ queryVars ] -> zip queryVars <$> relevantKnowledgeBaseSyms
    [] -> error $ "The query '" ++ predSym ++ "' doesn't exist."
    _  -> error $ "The query '" ++ predSym ++ "' has multiple clauses."
  where
  relevantKnowledgeBase = filter ((== predSym) . _predSym) $ solve pr
  relevantKnowledgeBaseSyms = _terms <$> relevantKnowledgeBase

  queryRules = filter ((== predSym) . _predSym . _head) pr
  queryVarsL = _terms . _head <$> queryRules 
