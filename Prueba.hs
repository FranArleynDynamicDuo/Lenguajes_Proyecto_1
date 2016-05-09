module Prueba where
-- Definimos Term
data Term = Var Int | Or Term Term | Neg Term | And Term Term | Equiv Term Term | UnEquiv Term Term | Imply Term Term

-- Definimos el Operador "\/""
infixl 5 \/
(\/) :: Term -> Term -> Term
(\/) t1 t2 = Or t1 t2

-- Definimos el Operador "/\""
infixl 5 /\
(/\) :: Term -> Term -> Term
(/\) t1 t2 = And t1 t2

-- Definimos el Operador negacion
neg :: Term -> Term
neg t1 = Neg t1

-- Definimos el Operador equivalencia
infixl 5 <==>
(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Equiv t1 t2

-- Definimos el Operador equivalencia
infixl 5 !<==>
(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = UnEquiv t1  t2

-- Definimos el Operador equivalencia
infixr 5 ==>
(==>) :: Term -> Term -> Term
(==>) t1 t2 = Imply t1 t2

x1 :: Term
x1 = Var 1

x2 :: Term
x2 = Var 2

x3 :: Term
x3 = Var 3

-- Muestra los terminos de manera presentable (Importante para el Proyecto)
showTerm :: Term -> String
showTerm (Var x) = "x" ++ (show x)

showTerm (Or (Var x) (Var y)) = showTerm(Var x) ++ "\\/" ++ showTerm(Var y)
showTerm (Or (Var x) t) = showTerm(Var x)  ++ "\\/ (" ++ showTerm t ++ ")"
showTerm (Or t (Var x)) = "(" ++ showTerm t ++ ")" ++ "\\/" ++ showTerm(Var x)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

showTerm (And (Var x) (Var y)) = showTerm(Var x) ++ "/\\" ++ showTerm(Var y)
showTerm (And (Var x) t) = showTerm(Var x)  ++ "/\\ (" ++ showTerm t ++ ")"
showTerm (And t (Var x)) = "(" ++ showTerm t ++ ")" ++ "/\\" ++ showTerm(Var x)
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"

showTerm (Equiv (Var x) (Var y)) = showTerm(Var x) ++ "<==>" ++ showTerm(Var y)
showTerm (Equiv (Var x) t) = showTerm(Var x)  ++ "<==> (" ++ showTerm t ++ ")"
showTerm (Equiv t (Var x)) = "(" ++ showTerm t ++ ")" ++ "<==>" ++ showTerm(Var x)
showTerm (Equiv t1 t2) = "(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2  ++ ")"

showTerm (UnEquiv (Var x) (Var y)) = showTerm(Var x) ++ "!<==>" ++ showTerm(Var y)
showTerm (UnEquiv (Var x) t) = showTerm(Var x)  ++ "!<==> (" ++ showTerm t ++ ")"
showTerm (UnEquiv t (Var x)) = "(" ++ showTerm t ++ ")" ++ "!<==>" ++ showTerm(Var x)
showTerm (UnEquiv t1 t2) = "(" ++ showTerm t1 ++ ") !<==> (" ++ showTerm t2 ++ ")"

showTerm (Imply (Var x) (Var y)) = showTerm(Var x) ++ "==>" ++ showTerm(Var y)
showTerm (Imply (Var x) t) = showTerm(Var x)  ++ "==> (" ++ showTerm t ++ ")"
showTerm (Imply t (Var x)) = "(" ++ showTerm t ++ ")" ++ "==>" ++ showTerm(Var x)
showTerm (Imply t1 t2) = "(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"

-- Importante si falta no corre el proyecto
instance Show Term where show = showTerm

-- SUSTITUCION TEXTUAL DEL OR (NOTA: Hay que hacerlo generalizado)
i = \x -> x
k = \x -> \y -> x
s = \x -> \y -> \z -> (x z) (y z)

abstraer :: Term -> Term -> Term -> Term
abstraer (Var x) (Var y) = if x == y then i else k (Var x)
abstraer (Var x) (Or (Var y) t2) = s ( s ( k Or ) i ) (k t2)

abstraer2 :: Term -> Term -> Term -> Term
abstraer2 (Var x) (Var y) = if x == y then i else k (Var x)
abstraer2 (Var x) (Or t1 t2) =  s ( s ( k Or ) (abstraer2 (Var x) t1) ) (abstraer2 (Var x) t2)
