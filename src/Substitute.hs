-- a module for representing substitutions
module Substitute where

import qualified TermF
import qualified Data.List.NonEmpty as LN
import qualified Formula

-- a finite set of bindings where a binding is
-- a pair v -> e where v is member of V (set of variables)
-- and e is a member of E (set of expressions)
type Subst = LN.NonEmpty (TermF.Id, TermF.Term)

-- domain of substitution
dom :: Subst -> (LN.NonEmpty TermF.Id)
dom = LN.map fst

-- range of substitution
rng :: Subst -> (LN.NonEmpty TermF.Term)
rng = LN.map snd

-- a restriction of a substitution S to a set of variables W
-- can be defined as removal of bindings whose variable is not contained
-- by the W
restriction :: (LN.NonEmpty TermF.Id) -> Subst -> Maybe Subst
restriction w s = let checkfn (varId, _) = elem varId w
                      someBindings = LN.filter checkfn s
                    in if null someBindings
                       then Nothing
                       else Just (LN.fromList someBindings)

-- application of substitution to an identifier
-- This function permits us to use substitution as a map between an identifier
-- and a term
appI :: Subst -> TermF.Id -> TermF.Term
appI s tId = let checkfn (varId, _) = varId == tId
                 someBindings = LN.filter checkfn s
             in if null someBindings
                then TermF.Var tId
                else snd (head someBindings)

appMi :: Maybe Subst -> TermF.Id -> TermF.Term
appMi Nothing tId = TermF.Var tId
appMi (Just s) tId = appI s tId

-- application of substitution from term to term
appT :: Subst -> TermF.Term -> TermF.Term
appT s (TermF.Var i)  = appI s i
appT s (TermF.Struct i ts)  = TermF.Struct i (appTs s ts)

appMt :: Maybe Subst -> TermF.Term -> TermF.Term
appMt Nothing t = t
appMt (Just s) t = appT s t

appTs :: Subst -> (LN.NonEmpty TermF.Term) -> (LN.NonEmpty TermF.Term)
appTs s ts = let apps t = appT s t
             in LN.map apps ts

appMts :: Maybe Subst -> (LN.NonEmpty TermF.Term) -> (LN.NonEmpty TermF.Term)
appMts Nothing ts = ts
appMts (Just s) ts = appTs s ts


-- application of substitution to a formula
appF :: Subst -> Formula.Form -> Formula.Form

-- start enumerating formula constructors
appF s (Formula.Atomic t) = Formula.Atom t (LN.fromList [(appI s t)])
appF s (Formula.Atom t ts) = Formula.Atom t (appTs s ts)
appF s (Formula.Eq a b) = Formula.Eq (appT s a) (appT s b)
appF s (Formula.Neg f) = Formula.Neg (appF s f)
appF s (Formula.Impl f1 f2) = Formula.Impl (appF s f1) (appF s f2)
appF s (Formula.Equi f1 f2) = Formula.Equi (appF s f1) (appF s f2)
appF s (Formula.Conj fs) = let appfn f = appF s f
                           in Formula.Conj (LN.map appfn fs)
appF s (Formula.Disj fs) = let appfn f = appF s f
                           in Formula.Disj (LN.map appfn fs)

appF s (Formula.Forall i f) = let restS = restriction (i LN.:| []) s
                      in appMf restS f

appF s (Formula.Exists i f) = let restS = restriction (i LN.:| []) s
                              in appMf restS f

appMf :: Maybe Subst -> Formula.Form -> Formula.Form
appMf Nothing f = f
appMf (Just s) f = appF s f
