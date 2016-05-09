-- List Theorems
-- Cambiar lanegacion por una funcion llamada "neg"
prop :: Float -> Equation
prop num
	| num == 3.1   = (p <==> q) <==> r === p <==> (q <==> r)
	| num == 3.2   = (p <==> q) <==> (q <==> p) === true
	| num == 3.3   = (p <==> q) <==> q === p
	| num == 3.4   = (true <==> p) === p
	| num == 3.5   = true
	| num == 3.8   = false === neg true
	| num == 3.9   = neg (p <==> q) === (neg p) <==> q
	| num == 3.10  = (p !<==> q) === neg(p <==> q)
	| num == 3.11  = (neg p) <==> q === p <==> (neg q)
	| num == 3.12  = (neg (neg p)) === p
	| num == 3.13  = (neg false) === true
	| num == 3.14  = (p !<==> q) === (neg p) <===> q  
	| num == 3.15  = ((neg p) <==> p) === false
	| num == 3.16  = (p !<==> q) === (q !<==> p)
	| num == 3.17  = (p !<==> q) !<==> r === p !<==> (q !<==> r)
	| num == 3.18  = (p !<==> q) <==> r === p !<==> (q <==> r)
	| num == 3.19  = p !<==> q <==> r === p !<==> q <==> r
	| num == 3.24  = p \/ q === q \/ p
	| num == 3.25  = (p \/ q) \/ r === p \/ (q \/ r)
	| num == 3.26  = p \/ p === p
	| num == 3.27  = p \/ (q <==> r) === p \/ q <==> p \/ r
	| num == 3.28  = p \/ (neg p)
	| num == 3.29  = p \/ true === true
	| num == 3.30  = p \/ false === p 
	| num == 3.31  = p \/ (q \/ r) === (p \/ q) \/ (p \/ r)
	| num == 3.32  = p \/ q <==> p \/ (neg q) === p
	| num == 3.35  = p /\ q <==> p === q <==> p \/ q
	| num == 3.36  = p /\ q === q /\ p
	| num == 3.37  = (p /\ q) /\ r === p /\ (q /\ r)
	| num == 3.38  = p /\ p === p
	| num == 3.39  = p /\ true === p
	| num == 3.40  = p /\ false === false
	| num == 3.41  = p /\ (q /\ r) === (p /\ q) /\ (p /\ q)
	| num == 3.42  = p /\ (neg p) === false
	| num == 3.431 = p /\ (q \/ q) === p
	| num == 3.432 = p \/ (p /\ q) === p
	| num == 3.441 = p /\ ((neg p) \/ q) === p /\ q
	| num == 3.442 = p \/ ((neg p) /\ q) === p \/ q
	| num == 3.45  = p \/ (q /\ r) === (p \/ q) /\ (p \/ r)
	| num == 3.46  = p /\ (q \/ r) === (p /\ q) \/ (q \/ r)
	| num == 3.471 = neg (p /\ q) === (neg p) \/ (neg q)
	| num == 3.472 = neg (p \/ q) === (neg p) /\ (neg q)
	| num == 3.48  = p /\ q <==> p /\ (neg q) === (neg p)
	| num == 3.49  = p /\ (q <==> r) <==> p /\ q <==> p /\ r === p
	| num == 3.50  = p /\ (q <==> p) === p /\ q
	| num == 3.51  = (p <==> q) /\ (r <==> p) === (p <==> q) /\ (r <==> q) 
	| num == 3.52  = p <==> q === (p /\ q) \/ ((neg p) /\ (neg q))
	| num == 3.53  = p !<==> q === ( (neg p) /\ q) \/ (p /\ (neg q))
	| num == 3.55  = (p /\ q) /\ r <==> p <==> q <==> r <==> p \/ q === q \/ r <==> r \/ p <==> p \/ q \/ r
	| num == 3.57  = p ==> q === p \/ q <==> q
	| num == 3.58  = p <== q === q ==>p 
	| num == 3.59  = p ==> q === (neg p) \/ q
	| num == 3.60  = p ==> q <==> p /\ q === p
	| num == 3.61  = p ==> q === (neg q) ==> (neg p)
	| num == 3.62  = p ==> (q <==> r) === p /\ q <==> p /\ r
	| num == 3.63  = p ==> (q <==> r) <==> p ==> q === p ==> r
	| num == 3.64  = p ==> (q ==> r ) === (p ==> q) ==> (p ==> r)
	| num == 3.65  = p /\ q ==> r === p ==> (q ==> r)
	| num == 3.66  = p /\ (p ==> q) === p /\ q
	| num == 3.67  = p /\ (q ==> p) === p
	| num == 3.68  = p \/ (p ==> q) === true
	| num == 3.69  = p \/ (q ==> p) === q ==> p
	| num == 3.70  = p \/ q ==> p /\ q === p <==> q
	| num == 3.71  = p ==> p === true
	| num == 3.72  = p ==> true === true
	| num == 3.73  = true ==> p === p
	| num == 3.74  = p ==> false === (neg p)
	| num == 3.75  = false ==> p === true
	| num == 3.761 = p ==> p \/ q 
	| num == 3.762 = p /\ q ==> p
	| num == 3.763 = p /\ q ==> p \/ q
	| num == 3.764 = p \/ (q /\ r) ==> p \/ q 
	| num == 3.765 = p /\ q ==> p /\ (q \/ r)
	| num == 3.77  = p /\ (p ==> q) ==> q
	| num == 3.78  = (p ==> r) /\ (q ==> r) === (p \/ q ==> r)
	| num == 3.79  = (p ==> r) \/ ( (neg p) ==> r) === r
	| num == 3.80  = (p ==> q) /\ (q ==> p) === (p <==> q)
	| num == 3.81  = (p ==> q) /\ (q ==> p) ==> (p <==> q)
	| num == 3.821 = (p ==> q) /\ (q ==> r) ==> (p ==> r)
	| num == 3.822 = (p <==> q) /\ (q ==> r) ==> (p ==> r)
	| num == 3.823 = (p ==> q) /\ (q <==> r) ==> (p ==> r)   
	| otherwise = error "The statement dofesn't exists"