module Theroem3_14 where
import Term
import Function
import Theorems

verify = let theorem = ( p !<==> q === neg p <==> q ) in
         proof theorem
         >>=
         statement 3.101 with (p =: p) using lambda z (z)
         >>=
         statement 3.9 with (p =: p) using lambda z (z)
         >>=
         done theorem
