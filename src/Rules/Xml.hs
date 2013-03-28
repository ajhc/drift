-- expanded from stub module to add new rules.
module Rules.Xml(rules) where

import Data.List (nub,sortBy)
import RuleUtils -- useful to have a look at this too

rules :: [RuleDef]
rules =
 [ ("Haskell2Xml", userRuleXmlOld, "Representation"
                            , "encode terms as XML (HaXml<=1.13)", Nothing)
 , ("XmlContent", userRuleXmlNew, "Representation"
                            , "encode terms as XML (HaXml>=1.14)", Nothing)
 , ("Parse", userRuleTextParse, "Utility"
                            , "parse values back from standard 'Show'"
                            , Just "Generates the Parse class supplied in\
				\ module Text.ParserCombinators.TextParser\
				\ as part of HaXml>=1.14.  This represents\
				\ a replacement for the Prelude.Read class,\
				\ with better error messages.")
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

userRuleXmlOld dat =
  let cs  = body dat		-- constructors
      cvs = mknss cs namesupply	-- variables
  in
  instanceheader "Haskell2Xml" dat $$
  block (toHTfn cs cvs dat
         : ( text "fromContents (CElem (Elem constr [] cs):etc)"
              $$ vcat (preorder cs (zipWith readsfn cvs cs)))
         : zipWith3 showsfn [0..] cvs cs)

userRuleXmlNew dat =
  let cs  = body dat		-- constructors
      cvs = mknss cs namesupply	-- variables
  in
  instanceheader "HTypeable" dat $$
  block [toHTfn cs cvs dat] $$
  instanceheader "XmlContent" dat $$
  block (
    case cs of
      [c] -> text "parseContents = do"
             $$ nest 4 (text "{ inElementWith (flip isPrefixOf)"
                       <+> text (show (constructor c)) <+> text "$"
                       $$ parseFn True (head cvs) c
                       $$ text "}"
                       )
      _ -> text "parseContents = do"
            $$ nest 4 (text "{ e@(Elem t _ _) <- elementWith (flip isPrefixOf)"
                      <+> text (show (preorder cs (map constructor cs)))
                      $$ text "; case t of"
                      $$ nest 2 (text "_"
                                $$ nest 2 (vcat (preorder cs
                                                       (zipWith (parseFn False)
                                                                cvs cs))))
                      $$ text "}"
                      )
    : zipWith3 showsfn [0..] cvs cs)

toHTfn cs cvs dat =
  let typ  = name dat
      fvs  = vars dat
      pats = concat (zipWith mkpat cvs cs)
  in
  text "toHType v =" $$
  nest 4 (
    text "Defined" <+>
    fsep [ text "\"" <> text typ <> text "\""
         , bracketList (map text fvs)
         , bracketList (zipWith toConstr cvs cs)
         ]
    ) $$
  if null pats then empty
  else nest 2 (text "where") $$
       nest 4 (vcat (map (<+> text "= v") pats)) $$
       nest 4 (vcat (map (simplest typ (zip cvs cs)) fvs))

namesupply   = [text [x,y] | x <- ['a' .. 'z'],
                             y <- ['a' .. 'z'] ++ ['A' .. 'Z']]

mknss []     _  = []
mknss (c:cs) ns =
  let (thisns,rest) = splitAt (length (types c)) ns
  in thisns: mknss cs rest

mkpat ns c =
  if null ns then []
  else [mypattern (constructor c) (types c) ns]


toConstr :: [Doc] -> Body -> Doc
toConstr ns c =
  let cn = constructor c
      ts = types c
      fvs = nub (concatMap deepvars ts)
  in
  text "Constr" <+>
  fsep [ text "\"" <> text cn <> text "\""
       , bracketList (map text fvs)
       , bracketList (map (\v-> text "toHType" <+> v) ns)
       ]

  where

    deepvars (Arrow t1 t2)  = []
    --deepvars (Apply t1 t2)  = deepvars t1 ++ deepvars t2
    deepvars (LApply c ts)  = concatMap deepvars ts
    deepvars (Var s)        = [s]
    deepvars (Con s)        = []
    deepvars (Tuple ts)     = concatMap deepvars ts
    deepvars (List t)       = deepvars t

--first [] fv = error ("cannot locate free type variable "++fv)
--first ((ns,c):cs) fv =
--  let npats = [ (n,pat) | (n,t) <- zip ns (types c)
--                        , (True,pat) <- [ find fv t ]
--              ]
--  in
--  if null npats then
--       first cs fv
--  else let (n,pat) = head npats
--       in parens pat <+> text "= toHType" <+> n
--
--  where
--
--    find :: String -> Type -> (Bool,Doc)
--    find v (Arrow t1 t2)  = (False,error "can't ShowXML for arrow type")
--    find v (Apply t1 t2)  = let (tf1,pat1) = find v t1
--                                (tf2,pat2) = find v t2
--                            in perhaps (tf1 || tf2)
--                                       (pat1 <+> snd (perhaps tf2 pat2))
--    find v (LApply c ts)  = let (_,cpat) = find v c
--                                tfpats = map (find v) ts
--                                (tfs,pats) = unzip tfpats
--                            in perhaps (or tfs)
--                                       (parens (cpat <+>
--                                                bracketList (map (snd.uncurry perhaps) tfpats)))
--    find v (Var s)        = perhaps (v==s) (text v)
--    find v (Con s)        = (False, text "Defined" <+>
--                                    text "\"" <> text s <> text "\"")
--    find v (Tuple ts)     = let tfpats = map (find v) ts
--                                (tfs,pats) = unzip tfpats
--                            in perhaps (or tfs)
--                                       (parens (text "Tuple" <+>
--                                                bracketList (map (snd.uncurry perhaps) tfpats)))
--    find v (List t)       = let (tf,pat) = find v t
--                            in perhaps tf (parens (text "List" <+> pat))
--    perhaps tf doc = if tf then (True,doc) else (False,text "_")

simplest typ cs fv =
  let npats = [ (depth,(n,pat)) | (ns,c) <- cs
                                , (n,t) <- zip ns (types c)
                                , (depth, pat) <- [ find fv t ]
              ]
      (_,(n,pat)) = foldl closest (Nothing,error "free tyvar not found") npats
  in
  parens pat <+> text "= toHType" <+> n

  where

    find :: String -> Type -> (Maybe Int,Doc)
    find v (Arrow t1 t2)  = (Nothing,error "can't derive Haskell2Xml/HTypeable for arrow type")
--    find v (Apply t1 t2)  = let (d1,pat1) = find v t1
--                                (d2,pat2) = find v t2
--                            in perhaps (combine [d1,d2])
--                                       (pat1 <+> snd (perhaps d2 pat2))
    find v (LApply c ts)
        | c == (Con typ)  = (Nothing, text "_")
        | otherwise       = let (_,cpat)  = find v c
                                dpats     = map (find v) ts
                                (ds,pats) = unzip dpats
                            in perhaps (combine ds)
                                       (cpat <+>
                                        bracketList (map (snd.uncurry perhaps) dpats) <+>
                                        text "_")
    find v (Var s)        = perhaps (if v==s then Just 0 else Nothing) (text v)
    find v (Con s)        = (Nothing, text "Defined" <+>
                                      text "\"" <> text s <> text "\"")
    find v (Tuple ts)     = let dpats = map (find v) ts
                                (ds,pats) = unzip dpats
                            in perhaps (combine ds)
                                       (text "Tuple" <+>
                                        bracketList (map (snd.uncurry perhaps) dpats))
    find v (List t)       = let (d,pat) = find v t
                            in perhaps (inc d) (text "List" <+> parens pat)

    perhaps Nothing doc   = (Nothing, text "_")
    perhaps jn doc        = (jn,doc)
    combine ds   = let js = [ n | (Just n) <- ds ]
                   in if null js then Nothing else inc (Just (minimum js))
    inc Nothing  = Nothing
    inc (Just n) = Just (n+1)

    closest :: (Maybe Int,a) -> (Maybe Int,a) -> (Maybe Int,a)
    closest (Nothing,_)  b@(Just _,_) = b
    closest a@(Just n,_) b@(Just m,_) | n< m  = a
                                      | m<=n  = b
    closest a b = a


-- showsfn (n = index) (ns = variables) (cn = constructor body)
showsfn n ns cn =
  let cons = constructor cn
      typ  = types cn
      sc   = parens (text "showConstr" <+> text (show n) <+>
                     parens (text "toHType" <+> text "v"))
      cfn []  = text "[]"
      cfn [x] = parens (text "toContents" <+> x)
      cfn xs  = parens (text "concat" <+> bracketList (map (text "toContents" <+>) xs))
  in
  text "toContents" <+>
  text "v@" <> mypattern cons typ ns <+> text "=" $$
  nest 4 (text "[mkElemC" <+> sc <+> cfn ns <> text "]")

----
--  text "fromContents (CElem (Elem constr [] cs):etc)" $$
----
-- readsfn (ns = variables) (cn = constructor body)
readsfn ns cn =
  let cons   = text (constructor cn)
      typ    = types cn
      num    = length ns - 1
      str d  = text "\"" <> d <> text "\""
      trails = take num (map text [ ['c','s',y,z] | y <- ['0'..'9']
                                                  , z <- ['0'..'9'] ])
      cfn x  = parens (text "fromContents" <+> x)
      (init,[last]) = splitAt num ns
      something = parens (
                    text "\\" <> parenList [last, text "_"] <> text "->" <+>
                    parens (cons <+> hsep ns <> text "," <+> text "etc") )
      mkLambda (n,cv) z = parens (
                            text "\\" <> parenList [n,cv] <> text "->" <+>
                            fsep [z, cfn cv] )
  in
  nest 4 (
    text "|" <+> str cons <+> text "`isPrefixOf` constr =" $$
    nest 4 (
      if null ns then parenList [cons, text "etc"]
      else fsep [ foldr mkLambda something (zip init trails)
                , cfn (text "cs")]
    )
  )
  -- Constructors are matched with "isPrefixOf" rather than "=="
  -- because of parametric polymorphism.  For a datatype
  --        data A x = A | B x
  -- the XML tags will be <A>, <B-Int>, <B-Bool>, <B-Maybe-Char> etc.
  -- However prefix-matching presents a problem for types like
  --        data C = C | CD
  -- because (C `isPrefixOf`) matches both constructors.  The solution
  -- (implemented by "preorder") is to order the constructors such that
  -- <CD> is matched before <C>.

preorder cs =
    map snd . reverse . sortBy (\(a,_) (b,_)-> compare a b) . zip (map constructor cs)


-- parseFn (ns = variables) (cn = constructor body)
parseFn single ns cn =
  let cons = constructor cn
      arity = length (types cn)
      var v = text ";" <+> v <+> text "<- parseContents"
      intro = if single then empty
              else text "|" <+> text (show cons)
                   <+> text "`isPrefixOf` t -> interior e $"
  in
  case arity of
    0 -> intro <+> nest 8 (text "return" <+> text cons)
    1 -> intro <+> nest 8 (text "fmap" <+> text cons <+> text "parseContents")
    _ -> intro $$  nest 8 (text "return" <+> text cons
                          <+> (fsep (replicate arity
                                               (text "`apply` parseContents"))))

--

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

mypattern :: Constructor -> [a] -> [Doc] -> Doc
mypattern c l ns =
  if null l then text c
  else parens (hsep (text c : take (length l) ns))


-- ----------------------------------------------------------------------- --
userRuleTextParse dat =
  let cs  = body dat		-- constructors
      cvs = mknss cs namesupply	-- variables
      isNullary c = null (types c)
  in
  instanceheader "Parse" dat $$
  nest 4 (
    case cs of
      []  -> empty
      _ | all isNullary cs ->
             text "parse = enumeration" <+> text (show (name dat))
             <+> text "["
             <+> fsep ( text (constructor (head cs))
                      : map (\c-> text "," <+> text (constructor c))
                           (tail cs))
             <+> text "]"
        | otherwise ->
             text "parse = constructors"
             $$ nest 4 (text "[" <+> textParseFn (head cvs) (head cs)
                       $$ vcat (zipWith (\cv c-> text "," <+> textParseFn cv c)
                                        (tail cvs) (tail cs))
                       $$ text "]"
                       )
  )

-- textParseFn (ns = variables) (cn = constructor body)
textParseFn ns cn =
  let cons = constructor cn
      arity = length (types cn)
      fields = labels cn
      doField f = text "`discard` isWord \",\" `apply` field" <+> text (show f)
  in
  fsep ( text "(" <+> text (show cons)
       : text ","
         <+> nest 2
             (case arity of
                0 -> text "return" <+> text cons
                1 | null fields ->
                     text "fmap" <+> text cons <+> text "parse"
                _ | null fields ->
                     text "return" <+> text cons
                     <+> (fsep (replicate arity (text "`apply` parse")))
                  | otherwise ->
                     text "return" <+> text cons
                     <+> fsep ( text "`discard` isWord \"{\" `apply` field"
                                             <+> text (show (head fields))
                              : map doField (tail fields)
                              ++ [text "`discard` isWord \"}\""]
                              )
             )
       : text ")"
       : [])


-- ----------------------------------------------------------------------- --
