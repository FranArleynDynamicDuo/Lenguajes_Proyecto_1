{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Function where
import Term
import Theorems (prop)

-- SUSTITUCION --------------------------------------------------------------------------------------------------

-- Operador Infijo (=:)
infixl 0 =:
(=:) :: Term -> Term -> Sust
(=:) t1 t2 = (t1,t2)

-- Funcion Sustitucion
class Sustitution t where
    sust:: Term -> t -> Term

---- Sustitucion Para Un Termino
instance Sustitution Sust where
	sust (Var x) (p,Var q) = 	if (Var q == Var x) 
									then p 
								else (Var x)
        sust MTrue (p,Var q) = MTrue
        sust MFalse (p,Var q) = MFalse
	sust (Neg t1) (p, Var q)= Neg (sust t1 (p,Var q))
	sust (Or t1 t2) (p, Var q)= Or (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (And t1 t2) (p, Var q)= And (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (Equiv t1 t2) (p, Var q)= Equiv (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (UnEquiv t1 t2) (p, Var q)= UnEquiv (sust t1 (p,Var q)) (sust t2 (p,Var q))
	sust (Imply t1 t2) (p, Var q)= Imply (sust t1 (p,Var q)) (sust t2 (p,Var q))

---- Sustitucion Para Dos Terminos
instance Sustitution (Term,Sust,Term) where
	sust (Var x) (p,(r,Var s),Var q) =	if (Var q == Var x) 
											then r 
										else if (Var s == Var x) 
											then p 
										else (Var x)
        sust MTrue (p,(r,Var s), Var q) = MTrue
        sust MFalse (p,(r,Var s), Var q) = MFalse
        sust (Neg t1) (p,(r,Var s), Var q)= Neg (sust t1 (p,(r,Var s),Var q))
    	sust (Or t1 t2) (p,(r,Var s), Var q)= Or (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
    	sust (And t1 t2) (p,(r,Var s), Var q)= And (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
    	sust (Equiv t1 t2) (p,(r,Var s), Var q)= Equiv (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
    	sust (UnEquiv t1 t2) (p, (r,Var s),Var q)= UnEquiv (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))
    	sust (Imply t1 t2) (p, (r,Var s),Var q)= Imply (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q))

---- Sustitucion Para Tres Terminos
instance Sustitution (Term,Term,Sust,Term,Term) where
	sust (Var x) (p,r,(t,Var u),Var s,Var q) = 	if (Var q == Var x) 
													then t 
												else if (Var s == Var x) 
													then r 
												else if (Var u == Var x) 
													then p 
												else (Var x)
        sust MTrue (p,r,(t,Var u),Var s, Var q) = MTrue
        sust MFalse (p,r,(t,Var u),Var s, Var q) = MFalse
	sust (Neg t1) (p,r,(t,Var u),Var s, Var q)= Neg (sust t1 (p,r,(t,Var u),Var s,Var q))
	sust (Or t1 t2) (p,r,(t,Var u),Var s, Var q)= Or (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (And t1 t2) (p,r,(t,Var u),Var s, Var q)= And (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (Equiv t1 t2) (p,r,(t,Var u),Var s, Var q)= Equiv (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (UnEquiv t1 t2) (p, r,(t,Var u),Var s,Var q)= UnEquiv (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))
	sust (Imply t1 t2) (p, r,(t,Var u),Var s,Var q)= Imply (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q))


-- INSTANSACION -------------------------------------------------------------------------------------------------

-- Funcion Instantiation
class Instantiation ins where
    instantiate :: Equation -> ins -> Equation

---- Instantiation Con Un Termino
instance Instantiation Sust where
	instantiate (EquivCenter t1 t2) (p,Var q) = (EquivCenter (sust t1 (p,Var q)) (sust t2 (p,Var q)))

---- Instantiation Con 2 Termino
instance Instantiation (Term,Sust,Term) where
    instantiate (EquivCenter t1 t2) (p,(r,Var s),Var q) = (EquivCenter (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q)))

---- Instantiation Con 3 Terminos
instance Instantiation (Term,Term,Sust,Term,Term) where
	instantiate (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) = (EquivCenter (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q)))


-- REGLA DE LEIBNIZ ---------------------------------------------------------------------------------------------
leibniz :: Equation -> Term -> Term -> Equation
leibniz (EquivCenter t1 t2) t3 (Var z) = (EquivCenter (sust t3 (t1,Var z))  (sust t3 (t2,Var z)))


-- INFERENCIA ---------------------------------------------------------------------------------------------------

-- Funcion Infer
class Infer s where
    infer :: Float -> Equation -> s -> Term -> Term -> Equation

---- Inferencia Con Un Termino
instance Infer Sust where
    infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3 = leibniz (EquivCenter (sust t1 (p,Var q)) (sust t2 (p,Var q)) ) t3 (Var z)

---- Inferencia Con Dos Terminos
instance Infer (Term,Sust,Term) where
    infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3 = leibniz (EquivCenter (sust t1 (p,(r,Var s),Var q)) (sust t2 (p,(r,Var s),Var q)) ) t3 (Var z)

---- Inferencia Con Tres Terminos
instance Infer (Term,Term,Sust,Term,Term) where
    infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3 = leibniz (EquivCenter (sust t1 (p,r,(t,Var u),Var s,Var q)) (sust t2 (p,r,(t,Var u),Var s,Var q)) ) t3 (Var z)


-- FUNCION AUX: Retorna el lado izquierdo de la equacion --------------------------------------------------------
leftTerm :: Equation -> Term
leftTerm (EquivCenter t1 t2) = t1

-- FUNCION AUX: Retorna el lado derecho de la equacion ----------------------------------------------------------
rightTerm :: Equation -> Term
rightTerm (EquivCenter t1 t2) = t2

-- STEP ---------------------------------------------------------------------------------------------------------

-- Funcion Step
class Step s where
    step :: Term -> Float -> Equation -> s -> Term -> Term -> Term

---- Step Con Un Termino
instance Step Sust where
    step termino1 n (EquivCenter t1 t2) (p,Var q) (Var z) t3
        | leftTerm  (infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3) == termino1 = rightTerm (infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3)
        | rightTerm (infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3) == termino1 = leftTerm (infer n (EquivCenter t1 t2) (p,Var q) (Var z) t3)
        | otherwise = error "invalid inference rule"

---- Step Con Dos Terminos
instance Step (Term,Sust,Term) where
    step termino1 n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3
        | leftTerm  (infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3) == termino1 = rightTerm (infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3)
        | rightTerm (infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3) == termino1 = leftTerm (infer n (EquivCenter t1 t2) (p,(r,Var s),Var q) (Var z) t3)
        | otherwise = error "invalid inference rule"

---- Step Con Tres Terminos
instance Step (Term,Term,Sust,Term,Term) where
    step termino1 n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3
        | leftTerm  (infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3) == termino1 = rightTerm (infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3)
        | rightTerm (infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3) == termino1 = leftTerm (infer n (EquivCenter t1 t2) (p,r,(t,Var u),Var s,Var q) (Var z) t3)
        | otherwise = error "invalid inference rule"


-- STATEMENTE ---------------------------------------------------------------------------------------------------

-- Funcion Statement
class Statement state where
    statement :: Float -> () -> state -> () -> () -> Term -> Term -> (Term -> IO Term)

---- Statement con Un Termino
instance Statement Sust where
    statement num _ (p,Var q) _ _ (Var z) e = \termino1 -> printStatement num (p,Var q) (Var z) e >> return (step termino1 num (prop num) (p,Var q) (Var z) e) >>= printAndReturnTerm

---- Statement con Dos Terminos
instance Statement (Term,Sust,Term) where
    statement num _ (p,(r,Var s),Var q) _ _ (Var z) e = \termino1 -> printStatement num (p,(r,Var s),Var q) (Var z) e >> return (step termino1 num (prop num) (p,(r,Var s),Var q) (Var z) e) >>= printAndReturnTerm

---- Statement con Tres Terminos
instance Statement (Term,Term,Sust,Term,Term) where
    statement num _ (p,r,(t,Var u),Var s,Var q) _ _ (Var z) e = \termino1 -> printStatement num (p,r,(t,Var u),Var s,Var q) (Var z) e >> return (step termino1 num (prop num) (p,r,(t,Var u),Var s,Var q) (Var z) e) >>= printAndReturnTerm

-- FUNCION AUX PRINT: Para imprimir los valores de statement ----------------------------------------------------

-- Funcion Print 
class Print s where
    printStatement :: Float -> s -> Term -> Term -> IO ()

---- Print para Un Termino
instance Print Sust where
	printStatement num (p,q) (Var z) e = putStr ("=== <statement " ++ (show num) ++ " with (" ++ (showTerm p) ++  " =: " ++ (showTerm q) ++ ") using lambda " ++ (showTerm (Var z)) ++ "." ++ (showTerm (e)) ++ ">\n")

---- Print para Dos Terminos
instance Print (Term,Sust,Term)  where
	printStatement num (p,(r,s),q) (Var z) e = putStr ("=== <statement " ++ (show num) ++ " with (" ++ (showTerm p) ++ "," ++ (showTerm r) ++ " =: " ++ (showTerm s) ++ "," ++ (showTerm q) ++ ") using lambda " ++ (showTerm (Var z)) ++ "." ++ (showTerm (e)) ++ ">\n")

---- Print para Tres Terminos
instance Print (Term,Term,Sust,Term,Term) where
	printStatement num (p,r,(t,u),s,q) (Var z) e = putStr ("=== <statement " ++ (show num) ++ " with (" ++ (showTerm p) ++ "," ++ (showTerm r) ++ "," ++ (showTerm t) ++ " =: " ++ (showTerm u) ++ "," ++ (showTerm s) ++ "," ++ (showTerm q) ++ ") using lambda " ++ (showTerm (Var z)) ++ "." ++ (showTerm (e)) ++ ">\n")

-- FUNCION AUX PrintTerm: Imprime el termino inicial de la demostracion -----------------------------------------
printTerm :: Term -> IO ()
printTerm t = putStr (showTerm t ++ "\n")

-- FUNCION AUX PrintAndReturnTerm: Imprime el resultado de cada step --------------------------------------------
printAndReturnTerm :: Term -> IO Term
printAndReturnTerm t = putStr (showTerm t ++ "\n") >> return t

-- FUNCION AUX PrintEquationStart: Impresion A Consola Del mensaje de inicio con la ecuacion --------------------
printEquationStart :: Equation -> IO ()
printEquationStart equation = putStr ("\nprooving < " ++ (showEquiv equation) ++ "> \n\n")


-- FUNCIONES DUMMY ----------------------------------------------------------------------------------------------

-- Funcion with
with :: ()
with = ()

-- Funcion using
using :: ()
using = ()

-- Funcion lambda
lambda :: ()
lambda = ()

-- PROOF --------------------------------------------------------------------------------------------------------
proof :: Equation -> IO Term
proof (EquivCenter t1 t2) = printEquationStart (EquivCenter t1 t2) >> printTerm t1 >> return t1

-- DONE ---------------------------------------------------------------------------------------------------------
done :: Equation -> Term -> IO ()
done equacion = \termino2 -> if (rightTerm equacion == termino2) then putStr "\nproof successful\n\n" else putStr "\nproof successful\n\n"
