module Term where

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
            | EquivCenter   Term Term

-- Definimos Equation
-- data Equation = E Term Term

-- NOTA: VERIFICAR LA PRECEDENCIA

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
(===) :: Term -> Term -> Term -- Debe devolver Equation
(===) t1 t2 = EquivCenter t1  t2

-- Muestra los terminos de manera presentable (Importante para el Proyecto)
showTerm :: Term -> String
showTerm (Var x) = init $ tail $ show x
showTerm MTrue = "true"
showTerm MFalse = "false"

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

showTerm (EquivCenter (Var x) (Var y)) = showTerm(Var x) ++ "===" ++ showTerm(Var y)
showTerm (EquivCenter (Var x) t) = showTerm(Var x)  ++ "=== (" ++ showTerm t ++ ")"
showTerm (EquivCenter t (Var x)) = "(" ++ showTerm t ++ ")" ++ "===" ++ showTerm(Var x)
showTerm (EquivCenter t1 t2) = "(" ++ showTerm t1 ++ ") === (" ++ showTerm t2 ++ ")"


-- Importante si falta no corre el proyecto
instance Show Term where show = showTerm

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