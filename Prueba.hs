-- Definimos Term
data Term = Var Int | Or Term Term | Neg Term | And Term Term

-- Definimos el Operador "\/""
(\/) :: Term -> Term -> Term 
(\/) t1 t2 = Or t1 t2

-- Definimos el Operador "/\""
(/\) :: Term -> Term -> Term 
(/\) t1 t2 = And t1 t2

-- Definimos el Operador negacion
neg :: Term -> Term
neg t1 = Neg t1

x1 :: Term
x1 = Var 1

x2 :: Term
x2 = Var 2

x3 :: Term
x3 = Var 3

-- Muestra los terminos de manera presentable (Importante para el Proyecto)
showTerm :: Term -> String
showTerm (Var i) = "x" ++ (show i)
showTerm (Or (Var i) (Var j)) = showTerm(Var i) ++ "\\/" ++ showTerm(Var j)
showTerm (Or (Var i) t) = showTerm(Var i)  ++ "\\/ (" ++ showTerm(t) ++ ")"
showTerm (Or t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ "\\/" ++ showTerm(Var i)
showTerm (Or t1 t2) = "(" ++ showTerm(t1) ++ ") \\/ (" ++ showTerm(t2) ++ ")"

-- Importante si falta no corre el proyecto
instance Show Term where show = showTerm

-- SUSTITUCION TEXTUAL DEL OR (NOTA: Hay que hacerlo generalizado)
i = \x -> x
k = \x -> \y -> x
s = \x -> \y -> \z -> (x z) (y z)

abstraer :: Term -> Term -> (Term -> Term)
abstraer (Var x) (Var y) = if x == y then i else k (Var x)
abstraer (Var x) (Or (Var y) t2) = s ( s ( k Or ) i ) (k t2)

abstraer2 :: Term -> Term -> (Term -> Term)
abstraer2 (Var x) (Var y) = if x == y then i else k (Var x)
abstraer2 (Var x) (Or t1 t2) =  s ( s ( k Or ) (abstraer2 (Var x) t1) ) (abstraer2 (Var x) t2)