
fun curry f x y  =  f (x, y)

fun uncurry f (x, y) = f x y

fun other_curry f x y  = f y x 			 

(* example  *)
(* tupled but we with it were curried  *)
fun range (i, j) = if i > j then [] else i :: range (i+1, j)

val countup = curry range 1		(* does not work (yet) *)

val xs = countup 7		(* [1, 2, 3, 4, 5, 6, 7]  *)

		 
