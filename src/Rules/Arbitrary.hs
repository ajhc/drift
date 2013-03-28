module Rules.Arbitrary(rules) where

import Data.List
import RuleUtils

rules = [
    ("Arbitrary", userRuleArbitrary, "Debugging", "Derive reasonable Arbitrary for QuickCheck", Nothing)
    ]

{- datatype that rules manipulate :-


data Data = D {	name :: Name,			 -- type's name
			constraints :: [(Class,Var)],
			vars :: [Var],		 -- Parameters
			body :: [Body],
			derives :: [Class],	 -- derived classes
			statement :: Statement}  -- type of statement
	   | Directive				 --|
	   | TypeName Name			 --| used by derive (ignore)
		deriving (Eq,Show)

data Body = Body { constructor :: Constructor,
		    labels :: [Name], -- [] for a non-record datatype.
		    types :: [Type]} deriving (Eq,Show)

data Statement = DataStmt | NewTypeStmt deriving (Eq,Show)

type Name = String
type Var = String
type Class = String
type Constructor = String

type Rule = (Tag, Data->Doc)

-}


-- useful helper things
instanceheader cls dat =
  let fv     = vars dat
      tycon  = name dat
      ctx    = map (\v-> text cls <+> text v)
      parenSpace = parens . hcat . sepWith space
  in
  hsep [ text "instance"
       , opt fv (\v -> parenList (ctx v) <+> text "=>")
       , text cls
       , opt1 (texts (tycon: fv)) parenSpace id
       , text "where"
       ]




-- begin here for Arbitrary derivation


userRuleArbitrary dat@D{name = name, vars = vars, body = body } = ins where
    ins = instanceheader "Arbitrary" dat $$ block [arb, coarb]
    arb :: Doc
    arb = text "arbitrary" <+> equals <+> text "do" <+>
            vcat [text ("x <- choose ((1::Int),"++show (length body)++")"),
                  text "case x of" $$ vcat alts]
    alts= zipWith alt [1..] body
    alt k (Body cons _ tys) = let vs = zipWith (\k _ -> "v"++show k) [1..] tys
                              in text ("  "++show k++" -> do ")
                             <+> vcat ((map (\v -> text (v++" <- arbitrary")) vs)
                                       ++ [text ("return ("++cons++" "++concat (intersperse " " vs)++")")])
    coarb = text "coarbitrary = error \"coarbitrary not yet supported\""
