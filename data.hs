{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Term where

-- TIPOS
-- Definimos Term
data Term =   Var           Char
            | MTrue
            | MFalse
            | Neg           Term
            | And           Term Term
            | Or            Term Term
            | Imply         Term Term
            | Equiv         Term Term
            | UnEquiv       Term Term
            deriving Eq
-- Definimos Equation
data Equation = EquivCenter Term Term
-- Definimos Sust
type Sust = (Term,Term)

-- OPERADORES
-- Definimos el Operador negacion
neg :: Term -> Term
neg t1 = Neg t1
-- Definimos el Operador "\/""
infixl 4 \/
(\/) :: Term -> Term -> Term
(\/) t1 t2 = Or t1 t2
-- Definimos el Operador "/\""
infixl 4 /\
(/\) :: Term -> Term -> Term
(/\) t1 t2 = And t1 t2
-- Definimos el Operador Inequivalencia
infixr 3 ==>
(==>) :: Term -> Term -> Term
(==>) t1 t2 = Imply t1 t2
-- Definimos el Operador equivalencia
infixl 2 !<==>
(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = UnEquiv t1  t2
-- Definimos el Operador equivalencia
infixl 2 <==>
(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Equiv t1 t2
infixl 1 ===
(===) :: Term -> Term -> Equation -- Debe devolver Equation
(===) t1 t2 = EquivCenter t1  t2

-- Muestra los terminos de manera presentable (Importante para el Proyecto)
-- Caso Constante
showTerm :: Term -> String
showTerm (Var x) = init $ tail $ show x
showTerm MTrue = "true"
showTerm MFalse = "false"
-- Caso Negacion
showTerm (Neg (Var x)) = "¬(" ++ showTerm(Var x) ++ ")"
showTerm (Neg t) = "¬(" ++ showTerm t ++ ")"
-- Caso Disjuncion
showTerm (Or (Var x) (Var y)) = showTerm(Var x) ++ "\\/" ++ showTerm(Var y)
showTerm (Or (Var x) t) = showTerm(Var x)  ++ "\\/ (" ++ showTerm t ++ ")"
showTerm (Or t (Var x)) = "(" ++ showTerm t ++ ")" ++ "\\/" ++ showTerm(Var x)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"
-- Caso Conjuncion
showTerm (And (Var x) (Var y)) = showTerm(Var x) ++ "/\\" ++ showTerm(Var y)
showTerm (And (Var x) t) = showTerm(Var x)  ++ "/\\ (" ++ showTerm t ++ ")"
showTerm (And t (Var x)) = "(" ++ showTerm t ++ ")" ++ "/\\" ++ showTerm(Var x)
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"
-- Caso Equivalencia
showTerm (Equiv (Var x) (Var y)) = showTerm(Var x) ++ "<==>" ++ showTerm(Var y)
showTerm (Equiv (Var x) t) = showTerm(Var x)  ++ "<==> (" ++ showTerm t ++ ")"
showTerm (Equiv t (Var x)) = "(" ++ showTerm t ++ ")" ++ "<==>" ++ showTerm(Var x)
showTerm (Equiv t1 t2) = "(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2  ++ ")"
-- Caso Inequivalencia
showTerm (UnEquiv (Var x) (Var y)) = showTerm(Var x) ++ "!<==>" ++ showTerm(Var y)
showTerm (UnEquiv (Var x) t) = showTerm(Var x)  ++ "!<==> (" ++ showTerm t ++ ")"
showTerm (UnEquiv t (Var x)) = "(" ++ showTerm t ++ ")" ++ "!<==>" ++ showTerm(Var x)
showTerm (UnEquiv t1 t2) = "(" ++ showTerm t1 ++ ") !<==> (" ++ showTerm t2 ++ ")"
-- Caso Implicacion
showTerm (Imply (Var x) (Var y)) = showTerm(Var x) ++ "==>" ++ showTerm(Var y)
showTerm (Imply (Var x) t) = showTerm(Var x)  ++ "==> (" ++ showTerm t ++ ")"
showTerm (Imply t (Var x)) = "(" ++ showTerm t ++ ")" ++ "==>" ++ showTerm(Var x)
showTerm (Imply t1 t2) = "(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"
-- Definimos que los terminos se mostraran con la funcion showTerm
instance Show Term where show = showTerm
-- Definimos que las ecuaciones se mostraran con la funcion showEquiv
showEquiv :: Equation -> String
showEquiv (EquivCenter t1 t2) = "(" ++ showTerm t1 ++ ") === (" ++ showTerm t2 ++ ")"
-- Definimos la forma de mostrar las ecuaciones
instance Show Equation where show = showEquiv

a :: Term
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e = Var 'e'

f :: Term
f = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i = Var 'i'

j :: Term
j = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m = Var 'm'

n :: Term
n = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u = Var 'u'

v :: Term
v = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y :: Term
y = Var 'y'

z :: Term
z = Var 'z'

true :: Term
true = MTrue

false :: Term
false = MFalse

-- SUSTITUCION
infixl 0 =:
(=:) :: Term -> Term -> Sust
(=:) t1 t2 = (t1,t2)

class Sustitution t where
	sust:: Term -> t -> Term

instance Sustitution Sust where 
	sust (Var x) (p,Var q) = if (Var q == Var x) then p else (Var x)
	sust (Neg t1) (p, Var q)= Neg (sust t1 (p,Var q))
	sust (Or t1 t2) (p, Var q)= Or (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (And t1 t2) (p, Var q)= And (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (Equiv t1 t2) (p, Var q)= Equiv (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (UnEquiv t1 t2) (p, Var q)= UnEquiv (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (Imply t1 t2) (p, Var q)= Imply (sust t1 (p,Var q)) (sust t2 (p,Var q))
		
instance Sustitution (Term,Sust,Term) where 
	sust (Var x) (p,(r,Var s),Var q) = if (Var q == Var x) then p else if (Var s == Var x) then r else (Var x)
	sust (Neg t1) (p,(r,Var s), Var q)= Neg (sust t1 (p,(r,Var s),Var q))
	sust (Or t1 t2) (p,(r,Var s), Var q)= Or (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
	sust (And t1 t2) (p,(r,Var s), Var q)= And (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
	sust (Equiv t1 t2) (p,(r,Var s), Var q)= Equiv (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
	sust (UnEquiv t1 t2) (p, (r,Var s),Var q)= UnEquiv (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
	sust (Imply t1 t2) (p, (r,Var s),Var q)= Imply (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
	
instance Sustitution (Term,Term,Sust,Term,Term) where 
	sust (Var x) (p,r,(t,Var u),Var s,Var q) = if (Var q == Var x) then p else if (Var s == Var x) then r else if (Var u == Var x) then t else (Var x)
	sust (Neg t1) (p,r,(t,Var u),Var s, Var q)= Neg (sust t1 (p,r,(t,Var u),Var s,Var q))
	sust (Or t1 t2) (p,r,(t,Var u),Var s, Var q)= Or (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (And t1 t2) (p,r,(t,Var u),Var s, Var q)= And (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (Equiv t1 t2) (p,r,(t,Var u),Var s, Var q)= Equiv (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (UnEquiv t1 t2) (p, r,(t,Var u),Var s,Var q)= UnEquiv (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (Imply t1 t2) (p, r,(t,Var u),Var s,Var q)= Imply (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
