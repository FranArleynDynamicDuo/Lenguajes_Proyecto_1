module Theroem3_20 where
import Term
import Function
import Theorems

verify = let theorem = ( neg p <==> q === p <==> neg q ) in
         proof theorem
         >>=
         statement 3.9 with (p =: p) using lambda z (z)
         >>=
         statement 3.2 with (p =: p) using lambda z (neg z)
         >>=
         statement 3.9 with (q, p =: p, q) using lambda z (z)
         >>=
         statement 3.2 with (neg q =: q) using lambda z (z)
         >>=
         done theorem
