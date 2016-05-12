module Funciones where

-- Aqui se encuentran la lista de Funciones

---------------funcion Sustitucion-------------------
--		NOTA: * Se debe crear  un tipo de dato "Sust"
--            * Definir el operador infijo (=:)
-- 		sust :: Term -> Term -> Sust

---------------funcion  Instanciacion---------------
--		NOTA: Recibe un 
--				* tipo Equation
--				* tipos Sust y 
--			  Devuelve 
--				* una nueva ecuacion Equation				
--		instantiate :: Equation -> Sust -> Equation 

---------------funcion  Leibniz----------------------
--		NOTA: Recibe
--				* Una ecuacion de tipo Equation
--              * Un termino E
--				* Una Variable z
--			  Devuelve
--				* Una ecuacion de tipo Equation
--		leibniz:: Equation -> E -> Var z -> Equation  

---------------funcion Inferencia--------------------
--		NOTA: Recibe
--				* Numero n
--				* Una ecuaciocion tipo Equation
--				* una sustitucion "sus" ?
--				* Una variable z
--				* Un termino E
--			  Devuelve:
--				* Una nueva ecuacion tipo Equation
--		infer:: Int -> Equation -> Sust -> Var z -> E -> Equation

---------------funcion Step--------------------
--		NOTA: Debe devolver un mensaje de error si el 
--			  resultado es igual al termino 
--		NOTA: Recibe
--				* termino 1
--				* todos los argumentos de la funcion infer
--			  Devuelve
--				* termino 2
--		step:: termino 1 -> argd step -> termino 2
--


--------------------FUNCIONES DUMMY---------------------

---------------funcion Statemente--------------------
-- 		statement :: Term -> IO Term

---------------funcion With--------------------------
--		with :: Term -> IO Term

---------------funcion Using-------------------------
--		using :: Term -> IO Term

---------------funcion Lambda------------------------
--		lambda :: Term -> IO Term





















