module TestTerm where

import TermRep
{-! global : Term !-}

data SortA = SortA1 SortB | SortA2
data SortB = SortB Integer SortA

