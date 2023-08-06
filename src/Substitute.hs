-- a module for representing substitutions
module Substitute where

import qualified TermF
import qualified Formula
import qualified Data.List.NonEmpty as LN

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

subsFilter :: Subst -> ((TermF.Id, TermF.Term) -> Bool) -> Maybe Subst
subsFilter s checkfn = let someBindings = LN.filter checkfn s
                       in if null someBindings
                          then Nothing
                          else Just (LN.fromList someBindings)


-- a restriction of a substitution S to a set of variables W
-- can be defined as removal of bindings whose variable is not contained
-- by the W
restriction :: (LN.NonEmpty TermF.Id) -> Subst -> Maybe Subst
restriction w s = let checkfn (varId, _) = elem varId w
                  in subsFilter s checkfn

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

-- composing substitutions can be represented as filters on binding sets. 
-- For example, 
-- let theta = {v_1 -> t_1, v_2 -> t_2, ..., v_n -> t_n} and
--     sigma = {w_1 -> r_1, w_2 -> r_2, ..., w_m -> r_n} be substitution
-- representations Their composition compose(theta, sigma) is another
-- substitution representation where we remove certain bindings.
-- The filtered bindings are w_1 -> theta(r_i) where theta(r_i) = w_i that is
-- bindings which map w_i to itself via theta (that is bindings where theta
-- maps to domain of sigma) and bindings of the type v_j -> t_j where v_j \in
-- {w_1, ..., w_m}, that is bindings in which sigma maps to range of theta.

-- clean up function for applying the filtering
cleanUp :: Subst -> Maybe Subst
cleanUp s = let checkfn (_, varT) = TermF.isVar varT
            in subsFilter s checkfn

-- composition of two substitutions
-- compose X,Y where X: A -> B and Y: C -> D
-- compose X, Y =  x(y(C))
-- meaning that we need to remove elements from D that are not in A
-- after the application of substitution Y
-- x(y(.)), the `.` must be a member of C and A
-- and the y(.) must be a member of D and A
-- the first condition is basically a domain intersection
-- the second condition is a clean up from applying x to range of y
compose :: Subst -> Subst -> Maybe Subst
compose x y = let yDom = dom y
                  checkfn1 (varId, _) = varId `notElem` yDom -- member of C
                  xyDomIntersection = subsFilter x checkfn1
                  appx (varYId, varYTerm) = (varYId, appT x varYTerm)
                  cleanedUp = cleanUp (LN.map appx y)
              in case xyDomIntersection of
                    Nothing -> cleanedUp
                    Just i -> case cleanedUp of 
                                Nothing -> Just i
                                Just s -> Just (LN.appendList i (LN.toList s))
