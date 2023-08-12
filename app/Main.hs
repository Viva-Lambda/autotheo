module Main (main) where

import Lib
import Substitute
import qualified TermF
import Formula
import qualified Data.List.NonEmpty as LN

-- exercise 03 from page 7:
-- give an implementation of freeVarsInForm that gives the list of variables
-- with free occurrence in a formula
-- Formula: (^)[P[x], \exists x (v)[R[x,y], S[x,y,z]]
-- Here P, R, S are predicates
-- (^) represents a conjunction
-- (v) represents a disjunction
-- \exists means there exists at least one
exercise03 :: [TermF.Id]
exercise03 = let x = TermF.Var (TermF.Id "x" 0)
                 y = TermF.Var (TermF.Id "y" 0)
                 z = TermF.Var (TermF.Id "z" 0)
                 px = Atom (TermF.Id "P[x]" 0) (x LN.:|[])
                 rxy = Atom (TermF.Id "R[x,y]", 0) (x LN.:|[y])
                 sxyz = Atom (TermF.Id "S[x,y,z]", 0) (x LN.:|[y, z])
                 disj = Disj (rxy LN.:|[sxyz]) 
                 exist = Exists (TermF.Id "x" 1) disj
                 conj = Conj (px LN.:|[exist]) 
             in freeVarsInForm conj


main :: IO ()
main = someFunc
