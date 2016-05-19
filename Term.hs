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
neg = Neg
-- Definimos el Operador "\/""
infixl 4 \/
(\/) :: Term -> Term -> Term
(\/) = Or
-- Definimos el Operador "/\""
infixl 4 /\
(/\) :: Term -> Term -> Term
(/\) = And
-- Definimos el Operador Inequivalencia
infixr 3 ==>
(==>) :: Term -> Term -> Term
(==>) = Imply
-- Definimos el Operador equivalencia
infixl 2 !<==>
(!<==>) :: Term -> Term -> Term
(!<==>) = UnEquiv
-- Definimos el Operador equivalencia
infixl 2 <==>
(<==>) :: Term -> Term -> Term
(<==>) = Equiv
infixl 1 ===
(===) :: Term -> Term -> Equation -- Debe devolver Equation
(===) = EquivCenter

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
