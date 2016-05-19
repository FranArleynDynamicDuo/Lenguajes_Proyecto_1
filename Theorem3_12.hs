module Theroem3_12 where
import Term
import Function
import Theorems

verify = let theorem = ( neg (neg p) === p ) in
         proof theorem
         >>=
         statement 3.11 with (p =: p) using lambda z (neg z)
         >>=
         statement 3.9 with (p <==> neg q, q =: q, p) using lambda z (z) -- Correccion (q, p <==> neg q =: q, p)
         >>=
         statement 3.2 with (neg q =: q) using lambda z (neg q <==> z)
         >>=
         statement 3.1 with (neg q, neg q, p =: p, q, r) using lambda z (z) -- Correccion (q, neg q, neg q =: p, q, r)
         >>=
         statement 3.6 with (neg q =: q) using lambda z (z)
         >>=
         done theorem
