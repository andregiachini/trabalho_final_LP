module Interpreter where 

import Lexer 
import Parser 

isValList :: [Expr] -> Bool
isValList [] = True
isValList (x:xs) = isValue x && isValList xs

isValue :: Expr -> Bool 
isValue BTrue         = True 
isValue BFalse        = True 
isValue (Num _)       = True 
isValue (Lam _ _ _)   = True 
isValue (Tuple es)    = isValList es 
isValue _             = False 

subst :: String -> Expr -> Expr -> Expr 
subst x s y@(Var v) = if x == v then s else y 
subst x s (Num n)   = Num n
subst x s BTrue     = BTrue 
subst x s BFalse    = BFalse 
subst x s (Lam y tp t1) = Lam y tp (subst x s t1)
subst x s (App t1 t2)   = App   (subst x s t1) (subst x s t2) 
subst x s (Add t1 t2)   = Add   (subst x s t1) (subst x s t2) 
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (And t1 t2)   = And   (subst x s t1) (subst x s t2) 
subst x s (Or t1 t2)    = Or    (subst x s t1) (subst x s t2)
subst x s (If e1 e2 e3) = If (subst x s e1) (subst x s e2) (subst x s e3)
subst x s (Paren e)     = Paren (subst x s e)
subst x s (Tuple es)    = Tuple (map (subst x s) es)
subst x s (Proj i e)    = Proj i (subst x s e)

step :: Expr -> Expr 

step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2)       = Add (Num n1) (step e2)
step (Add e1 e2)             = Add (step e1) e2 

step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2)       = Times (Num n1) (step e2)
step (Times e1 e2)             = Times (step e1) e2

step (And BFalse _)  = BFalse 
step (And BTrue e2)  = e2 
step (And e1 e2)     = And (step e1) e2 

step (Or BTrue _)    = BTrue
step (Or BFalse e2)  = e2
step (Or e1 e2)      = Or (step e1) e2

step (If BTrue  e2 _) = e2
step (If BFalse _ e3) = e3
step (If e1 e2 e3)    = If (step e1) e2 e3

step (App (Lam x tp e1) e2) 
  | isValue e2              = subst x e2 e1 
  | otherwise               = App (Lam x tp e1) (step e2)
step (App e1 e2)            = App (step e1) e2

step (Paren e) = e

step (Tuple es) = Tuple (stepList es)

step (Proj i (Tuple es)) 
  | isValList es = es !! i
step (Proj i e) = Proj i (step e)

step e = error ("Stuck term: " ++ show e)

stepList :: [Expr] -> [Expr]
stepList [] = []
stepList (e:es) 
  | not (isValue e) = (step e) : es   
  | otherwise       = e : (stepList es) 

eval :: Expr -> Expr
eval e = if isValue e then e else eval (step e)