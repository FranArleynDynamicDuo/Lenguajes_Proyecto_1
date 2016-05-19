module Theorem3_5
    (
    ) where
import Term
import Function
import Theorems

verify = let theorem = ( neg false === true ) in
         proof theorem
         >>=
         statement 3.8 with (p =: p) using lambda z (neg z)
         >>=
         statement 3.12 with (true =: p) using lambda z (z)
         >>=
         done theorem
