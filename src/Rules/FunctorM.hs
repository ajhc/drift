-- stub module to add your own rules.
module Rules.FunctorM (rules) where

import Data.List
import RuleUtils

rules = [
    ("FunctorM", userRuleFunctorM, "Generics", "derive reasonable fmapM implementation", Nothing),
    ("Functor", userRuleFunctor, "Generics", "derive reasonable Functor instance", Nothing),
    ("Foldable", userRuleFoldable, "Generics", "derive instance for Data.Foldable", Nothing),
    ("Traversable", userRuleTraversable, "Generics", "derive instance for Data.Traversable", Nothing),
    ("RMapM", userRuleRMapM, "Generics", "derive reasonable rmapM implementation", Nothing)
    ]


hasVar tt t = hasType (Var tt) t

hasType tt t = has t where
    has t | t == tt = True
    has (List t) = has t
    has (Arrow a b) = has a || has b
    has (LApply t ts) = any has (t:ts)
    has (Tuple ts) = any has (ts)
    has _ = False



userRuleFoldable D{name = name, vars = [] } = text "--" <+> text name <> text ": Cannot derive Foldable without type variables"
userRuleFoldable D{name = name, vars = vars, body=body } = ins where
    (tt:rt') = reverse vars
    rt = reverse rt'
    fn = if null rt then text name else parens (text name <+> hsep (map text rt))
    ins = text "instance" <+> text "Foldable" <+> fn <+> text "where" $$ block fs
    fs = map f' $ body
    combine xs = if null xs then text "Data.Monoid.mempty" else hsep (intersperse (text "`Data.Monoid.mappend`") xs)
    f' Body{constructor=constructor, types=types} = text "foldMap" <+> f <+> pattern constructor types <+> equals <+>  combine (concatMap g (zip types vnt)) where
        vnt = varNames types
        g (t,n) | not (hasVar tt t) = []
        g (Var t,n) | t == tt = [parens $ f <+> n]
        g (List (Var t),n) | t == tt =  [parens $ text "Data.Monoid.mconcat $ Prelude.map" <+> f <+> n]
        g (List t,n)  = [parens $ text "Data.Monoid.mconcat $ Prelude.map" <+> lf t <+> n]  where
            lf t = parens $ text "\\x ->" <+> combine (g (t,x))
        g (LApply t [],n) = g (t,n)
        g (LApply t ts,n)  = [parens $ text "Data.Foldable.foldMap" <+> f <+> n]
        g (Tuple ts,n) = [parens $ text "case" <+> n <+> text "of" <+> tuple (varNames ts) <+> rArrow <+> combine (concatMap g (zip ts (varNames ts)))]
        g _ = []

userRuleFunctor D{name = name, vars = [] } = text "--" <+> text name <> text ": Cannot derive Functor without type variables"
userRuleFunctor D{name = name, vars = vars, body=body } = ins where
    (tt:rt') = reverse vars
    rt = reverse rt'
    fn = if null rt then text name else parens (text name <+> hsep (map text rt))
    ins = text "instance" <+> text "Functor" <+> fn <+> text "where" $$ block fs
    fs = map f' $ body
    f' Body{constructor=constructor, types=types} = text "fmap" <+> text "f" <+> pattern constructor types <+> equals <+> text constructor <+>  hsep (map g (zip types vnt)) where
        vnt = varNames types
        g (t,n) | not (hasVar tt t) = n
        g (Var t,n) | t == tt = parens $ f <+> n
        g (List (Var t),n) | t == tt =  parens $ text "Prelude.map" <+> f <+> n
        g (List t,n)  = parens $ text "Prelude.map" <+> lf t <+> n  where
            lf t = parens $ text "\\x ->" <+> g (t,x)
        g (LApply t [],n) = g (t,n)
        g (LApply t ts,n) | last ts == Var tt = parens $ text "fmap" <+> f <+> n
        g (Tuple ts,n) = parens $ text "case" <+> n <+> text "of" <+> tuple (varNames ts) <+> rArrow <+> tuple (map g (zip ts (varNames ts)))
        g _ = empty

userRuleFunctorM D{name = name, vars = [] } = text "--" <+> text name <> text ": Cannot derive FunctorM without type variables"
userRuleFunctorM D{name = name, vars = vars, body=body } = ins where
    (tt:rt') = reverse vars
    rt = reverse rt'
    fn = if null rt then text name else parens (text name <+> hsep (map text rt))
    ins = text "instance" <+> text "FunctorM" <+> fn <+> text "where" $$ block fs
    fs = map f' $ body
    f' Body{constructor=constructor, types=types} = text "fmapM" <+> text "f" <+> pattern constructor types <+> equals <+> text "do" <+> hcat (map g (zip types vnt)) <+> text "return $" <+> text constructor <+> hsep vnt where
        vnt = varNames types
        g (t,n) | not (hasVar tt t) = empty
        g (Var t,n) | t == tt = n <+> lArrow <+> text "f" <+> n <> semicolon
        g (List (Var t),n) | t == tt = n <+> lArrow <+> text "mapM" <+> f <+> n <> semicolon
        g (List t,n)  = n <+> lArrow <+> text "mapM" <+> lf t <+> n <> semicolon  where
            lf t = parens $ text "\\x ->" <+> text "do" <+> g (t,x) <+> text "return" <+> x
        g (LApply t [],n) = g (t,n)
        g (LApply t ts,n) | last ts == Var tt = n <+> lArrow <+> text "fmapM" <+> f <+> n <> semicolon
        g (Tuple ts,n) = n <+> lArrow <+> (parens $ text "do" <+> tuple (varNames ts) <+> lArrow <+> text "return" <+> n <> semicolon  <+> hcat (map g (zip ts (varNames ts))) <> text "return" <+> tuple (varNames ts)) <> semicolon
        g _ = empty

userRuleRMapM D{name = name, vars = vars, body=body } = ins where
    --(tt:rt') = reverse vars
    tt = if null vars then Con name else LApply (Con name) (map Var vars)
    rt = vars
    fn = if null rt then text name else parens (text name <+> hsep (map text rt))
    ins = text "instance" <+> text "RMapM" <+> fn <+> text "where" $$ block fs
    fs = map f' $ body
    f' Body{constructor=constructor, types=types} = text "rmapM" <+> text "f" <+> pattern constructor types <+> equals <+> text "do" <+> hcat (map g (zip types vnt)) <+> text "return $" <+> text constructor <+> hsep vnt where
        vnt = varNames types
        g (t,n) | not (hasType tt t) = empty
        g ( t,n) | t == tt = n <+> lArrow <+> text "f" <+> n <> semicolon
        g (List (t),n) | t == tt = n <+> lArrow <+> text "mapM" <+> f <+> n <> semicolon
        g (List t,n)  = n <+> lArrow <+> text "mapM" <+> lf t <+> n <> semicolon  where
            lf t = parens $ text "\\x ->" <+> text "do" <+> g (t,x) <+> text "return" <+> x
        g (LApply t [],n) = g (t,n)
        g (LApply t ts,n) | last ts ==  tt = n <+> lArrow <+> text "fmapM" <+> f <+> n <> semicolon
        g (Tuple ts,n) = n <+> lArrow <+> (parens $ text "do" <+> tuple (varNames ts) <+> lArrow <+> text "return" <+> n <> semicolon  <+> hcat (map g (zip ts (varNames ts))) <> text "return" <+> tuple (varNames ts)) <> semicolon
        g _ = empty

userRuleTraversable D{name = name, vars = [] } = text "--" <+> text name <> text ": Cannot derive Traversable without type variables"
userRuleTraversable D{name = name, vars = vars, body=body } = ins where
    (tt:rt') = reverse vars
    rt = reverse rt'
    fn = if null rt then text name else parens (text name <+> hsep (map text rt))
    ins = text "instance" <+> text "Data.Traversable.Traversable" <+> fn <+> text "where" $$ block fs
    fs = map f' $ body
    combine xs = if null xs then empty else text "<$>" <+> hsep (intersperse (text "<*>") xs)
    f' Body{constructor=constructor, types=types} = text "traverse" <+> f <+> pattern constructor types <+> equals <+> text constructor <+> combine (map g (zip types vnt)) where
        vnt = varNames types
        g (t,n) | not (hasVar tt t) = text "Control.Applicative.pure" <+> n
        g (Var t,n) | t == tt = f <+> n
        g (List (Var t),n) | t == tt = text "traverse" <+> f <+> n
        g (List t,n)  = text "traverse" <+> lf t <+> n  where
            lf t = parens $ text "\\x ->" <+> g (t,x)
        g (LApply t [],n) = g (t,n)
        g (LApply t ts,n) | last ts == Var tt = text "traverse" <+> f <+> n
--        g (Tuple ts,n) = (parens $  tuple (varNames ts) <+> lArrow <+> text "return" <+> n <> semicolon  <+> hcat (map g (zip ts (varNames ts))) <> text "return" <+> tuple (varNames ts)) <> semicolon
        g (Tuple ts,n) = parens $ text "case" <+> n <+> text "of" <+> tuple (varNames ts) <+> rArrow <+> text ("(" ++ replicate (length ts - 1) ',' ++ ")") <+> combine (map g (zip ts (varNames ts)))
        g _ = empty


