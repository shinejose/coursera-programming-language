(* 'a list * 'a list -> 'a list  *)
fun append (xs, ys) =
    case xs of
	[] => ys
      | x :: xs'  =>  x :: append (xs', ys) 

val ok1 = append (["hi","bye"], ["programming", "languages"])

val ok2 = append ([1, 2], [4, 5]);


(* val not ok = append ([1, 2], ["programming", "languages"])  *)
