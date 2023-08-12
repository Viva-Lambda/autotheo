module TermF (Name, Index, Id(..), Term(..), 
              isVar, varsInTerm, varsInTerms) where

import qualified Data.List.NonEmpty as LN
import Utils

type Name = String
type Index = [Int]

data Id = Id Name Index deriving (Eq, Ord)

instance Show Id where
    show (Id name []) = name
    show (Id name [i]) = name ++ ('_': show i)
    show (Id name ixs) = name ++ ('_': showixs ixs)
        where showixs [] = ""
              showixs (i:xs) = (show i) ++ showixs xs


data Term = Var Id
          | Struct Id (LN.NonEmpty Term) deriving (Eq)

instance Ord Term where
    -- variable to variable comparison
    compare (Var x) (Var y) = compare x y
    -- variable to struct comparison
    compare (Var _) _ = LT
    -- struct to variable comparison
    compare _ (Var _) = GT
    -- struct to struct comparison
    compare (Struct idx x) (Struct idy y) = if idx == idy 
                                            then compare x y
                                            else compare idx idy


instance Show Term where
    show (Var i) = show i
    show (Struct i xs) = show i ++ concat [ show xs ]


-- basic checking
isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

varsInTerm :: Term -> (LN.NonEmpty Id)
varsInTerm (Var i) = i LN.:| []
varsInTerm (Struct _ ts) = varsInTerms ts

varsInTerms :: (LN.NonEmpty Term) -> (LN.NonEmpty Id)
varsInTerms xs = let mapped = LN.map varsInTerm xs
                     joined = concatLN mapped
                 in LN.nub joined
