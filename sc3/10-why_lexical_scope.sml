fun f1 y =
    let
	val x = y +1
    in
	fn z => x + y + z
    end

fun f2 y =
    let
	val q = y +1
    in
	fn z => q + y + z
    end


val x = 17
val a1 = (f1 7) 4
val a2 = (f2 7) 4 		
			    

fun filter (f, xs) =
    case xs of
	[] => []
     | x::xs'  => if f x  then  x::(filter(f, xs')) else filter (f, xs') 

fun greaterThanX x = fn y => y > x

fun noNegatives xs = filter (greaterThanX ~1, xs)

fun allGreater (xs, n) = filter (fn x => x > n ,xs ) 			    
