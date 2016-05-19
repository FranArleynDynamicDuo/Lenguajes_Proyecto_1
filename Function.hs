{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Function where
import Term
import Theorem

-- SUSTITUCION
infixl 0 =:
(=:) :: Term -> Term -> Sust
(=:) t1 t2 = (t1,t2)

class Sustitution t where
    sust:: Term -> t -> Term

instance Sustitution Sust where
	sust (Var x) (p,Var q) = if (Var q == Var x) then p else (Var x)
        sust MTrue (p,Var q) = MTrue
        sust MFalse (p,Var q) = MFalse
	sust (Neg t1) (p, Var q)= Neg (sust t1 (p,Var q))
	sust (Or t1 t2) (p, Var q)= Or (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (And t1 t2) (p, Var q)= And (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (Equiv t1 t2) (p, Var q)= Equiv (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (UnEquiv t1 t2) (p, Var q)= UnEquiv (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (Imply t1 t2) (p, Var q)= Imply (sust t1 (p,Var q)) (sust t2 (p,Var q))


instance Sustitution (Term,Sust,Term) where
	sust (Var x) (p,(r,Var s),Var q) = if (Var q == Var x) then p else if (Var s == Var x) then r else (Var x)
        sust MTrue (p,(r,Var s), Var q) = MTrue
        sust MFalse (p,(r,Var s), Var q) = MFalse
        sust (Neg t1) (p,(r,Var s), Var q)= Neg (sust t1 (p,(r,Var s),Var q))
    	sust (Or t1 t2) (p,(r,Var s), Var q)= Or (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
    	sust (And t1 t2) (p,(r,Var s), Var q)= And (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
    	sust (Equiv t1 t2) (p,(r,Var s), Var q)= Equiv (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
    	sust (UnEquiv t1 t2) (p, (r,Var s),Var q)= UnEquiv (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
    	sust (Imply t1 t2) (p, (r,Var s),Var q)= Imply (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))

instance Sustitution (Term,Term,Sust,Term,Term) where
	sust (Var x) (p,r,(t,Var u),Var s,Var q) = if (Var q == Var x) then p else if (Var s == Var x) then r else if (Var u == Var x) then t else (Var x)
        sust MTrue (p,r,(t,Var u),Var s, Var q) = MTrue
        sust MFalse (p,r,(t,Var u),Var s, Var q) = MFalse
	sust (Neg t1) (p,r,(t,Var u),Var s, Var q)= Neg (sust t1 (p,r,(t,Var u),Var s,Var q))
	sust (Or t1 t2) (p,r,(t,Var u),Var s, Var q)= Or (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (And t1 t2) (p,r,(t,Var u),Var s, Var q)= And (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (Equiv t1 t2) (p,r,(t,Var u),Var s, Var q)= Equiv (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (UnEquiv t1 t2) (p, r,(t,Var u),Var s,Var q)= UnEquiv (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (Imply t1 t2) (p, r,(t,Var u),Var s,Var q)= Imply (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))

class Instantiation ins where
    instantiate :: Equation -> ins -> Equation

-- Instanciacion Con Un Termino
instance Instantiation Sust where
	instantiate (EquivCenter t1 t2) (p,Var q) = (EquivCenter (sust t1 (p,Var q)) (sust t2 (p,Var q)))
-- Instanciacion Con 2 Termino
instance Instantiation (Term,Sust,Term) where
    instantiate (EquivCenter t1 t2) (p,(r,Var s),Var q) = (EquivCenter (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q)))
-- Instanciacion Con 3 Terminos
instance Instantiation (Term,Term,Sust,Term,Term) where
	instantiate (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) = (EquivCenter (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q)))

-- Regla De Leibniz
leibniz :: Equation -> Term -> Term -> Equation
leibniz (EquivCenter t1 t2) t3 (Var z) = (EquivCenter (sust t3 (t1,Var z))  (sust t3 (t2,Var z)))

-- Inferencia
class Infer s where
    infer :: Float -> Equation -> s -> Term -> Term -> Equation


-- Instanciacion Con Un Termino
instance Infer Sust where
    infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3 = leibniz (EquivCenter (sust t1 (p,Var q)) (sust t2 (p,Var q)) ) t3 (Var z)

-- Step Con 2 Terminos
instance Infer (Term,Sust,Term) where
    infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3 = leibniz (EquivCenter (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q)) ) t3 (Var z)

-- Step Con 3 Terminos
instance Infer (Term,Term,Sust,Term,Term) where
    infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3 = leibniz (EquivCenter (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q)) ) t3 (Var z)




-- Funcion que retorna el lado izquierdo de una ecuacion
leftTerm :: Equation -> Term
leftTerm (EquivCenter t1 t2) = t1
-- Funcion que retorna el lado derecho de una equacion
rightTerm :: Equation -> Term
rightTerm (EquivCenter t1 t2) = t2

-- Step
class Step s where
    step :: Term -> Float -> Equation -> s -> Term -> Term -> Term

-- Instanciacion Con Un Termino
instance Step Sust where
    step termino1 n (EquivCenter t1 t2) (p,Var q) (Var z) t3
        | leftTerm  (infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3) == termino1 = rightTerm (infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3)
        | rightTerm (infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3) == termino1 = leftTerm (infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3)
        | otherwise = error "Error"

-- Step Con 2 Terminos
instance Step (Term,Sust,Term) where
    step termino1 n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3
        | leftTerm  (infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3) == termino1 = rightTerm (infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3)
        | rightTerm (infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3) == termino1 = leftTerm (infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3)
        | otherwise = error "Error"

-- Step Con 3 Terminos
instance Step (Term,Term,Sust,Term,Term) where
    step termino1 n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3
        | leftTerm  (infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3) == termino1 = rightTerm (infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3)
        | rightTerm (infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3) == termino1 = leftTerm (infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3)
        | otherwise = error "Error"

-- Statement
statement :: Float -> () -> Sust -> () -> () -> Term -> Term -> I0 Term
statement num _ sustitution _ _ (Var z) e = step t1 num (prop num) sustitution (Var z) e


with :: ()
with = ()
using :: ()
using = ()
lambda :: ()
lambda = ()


-- proof 
--proof :: Equation -> I0 Term
--(EquivCenter t1 t2) >>= statement = statement t1

-- verify :: ()
--done :: Equation -> Term -> String
--done equacion termino2
--    | (rightTerm equacion == termino2) = "proof successful"
--    | (rightTerm equacion == termino2) = "proof fail"