module Rules.Binary(rules) where

import Data.List (nub,intersperse)
import RuleUtils

rules = [
    ("Binary", userRuleBinary, "Binary", "Data.Binary binary encoding of terms", Nothing)
    ]



-- useful helper things
namesupply   = [text [x,y] | x <- ['a' .. 'z'],
                             y <- ['a' .. 'z'] ++ ['A' .. 'Z']]
mknss []     _  = []
mknss (c:cs) ns =
  let (thisns,rest) = splitAt (length (types c)) ns
  in thisns: mknss cs rest

mkpattern :: Constructor -> [a] -> [Doc] -> Doc
mkpattern c l ns =
  if null l then text c
  else parens (hsep (text c : take (length l) ns))

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




-- begin here for Binary derivation


userRuleBinary dat =
  let cs  = body dat
      cvs = mknss cs namesupply
      --k   = (ceiling . logBase 256 . realToFrac . length) cs
      k = length cs
  in
  instanceheader "Data.Binary.Binary" dat $$
  block (  zipWith3 (putfn k) [0..] cvs cs
        ++ [getfn k [0..] cvs cs]
        )

putfn 1 _ [] c =
    text "put" <+> ppCons [] c <+> text "= return ()"
putfn 1 _ cv c =
  text "put" <+> ppCons cv c <+> text "= do" $$
  nest 8 (
    vcat (map (text "Data.Binary.put" <+>) cv)
  )
putfn _ n cv c =
  text "put" <+> ppCons cv c <+> text "= do" $$
  nest 8 (
    text "Data.Binary.putWord8" <+> text (show n) $$
    vcat (map (text "Data.Binary.put" <+>) cv)
  )

ppCons cv c = mkpattern (constructor c) (types c) cv

getfn _ _ [[]] [c] =
    text "get = return" <+> ppCons [] c
getfn _ _ [vs] [c] =
  text "get = do" $$
    vcat (map (\v-> v <+> text "<-" <+> text "get") vs) $$
    text "return" <+> ppCons vs c
getfn _ ns cvs cs =
  text "get = do" $$
  nest 8 (
    text "h <- Data.Binary.getWord8"  $$
    text "case h of" $$
    nest 2 ( vcat $
      zipWith3 (\n vs c-> text (show n) <+> text "-> do" $$
                          nest 6 (
                            vcat (map (\v-> v <+> text "<-" <+> text "Data.Binary.get") vs) $$
                            text "return" <+> ppCons vs c
                          ))
               ns cvs cs ++ [ text "_ -> fail \"invalid binary data found\"" ]
    )
  )


