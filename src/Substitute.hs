-- a module for representing substitutions
module Substitute where

import qualified TermF
import qualified Data.List.NonEmpty as LN
import Data.List

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
restriction :: (LN.NonEmpty TermF.Id) -> Subst -> Subst
restriction ids s = let idlst = LN.toList ids
                        checkfn i = elem i idlst
                    in LN.filter checkfn s
