module Formula(Form(..)) where

import qualified TermF
import qualified Data.List.NonEmpty as LN
import Data.List

data Form = -- rules for the predicate logic
            -- if A is an Atomic (just an identifier), then A is a formula
            Atomic TermF.Id 
            -- if A is an Atom with a term, then A is a formula
          | Atom TermF.Id (LN.NonEmpty TermF.Term)
          | Eq TermF.Term TermF.Term
            -- if A is a formula, then its negation is also a formula
          | Neg Form
          | Impl Form Form
          | Equi Form Form
          -- a conjunction over a list of formula is a formula
          | Conj (LN.NonEmpty Form)
          -- a disjunction over a list of formula is a formula
          | Disj (LN.NonEmpty Form)
          | Forall TermF.Id Form
          | Exists TermF.Id Form
          deriving (Eq, Ord)

-- show implementation
instance Show Form where
    show (Atomic i) = show i
    show (Atom i terms) = (show i) ++ concat [ show terms ]
    show (Eq t1 t2) = (show t1) ++ "==" ++ (show t2)
    show (Neg f) = '~': (show f)
    show (Impl f1 f2) = "(" ++ (show f1) ++ "==>" ++ (show f2) ++ ")"
    show (Equi f1 f2) = "(" ++ (show f1) ++ "<=>" ++ (show f2) ++ ")"
    show (Conj fs) = "Conj(" ++ showFormLst fs ++ ")"
    show (Disj fs) = "Disj(" ++ showFormLst fs ++ ")"
    show (Forall i f) = "Forall " ++ (show i) ++ (' ': show f)
    show (Exists i f) = "Exists " ++ (show i) ++ (' ': show f)

showFormLst :: LN.NonEmpty Form -> String
showFormLst fs = let strs = LN.toList ( LN.map show fs)
                     commas = intercalate "," strs
                 in commas
