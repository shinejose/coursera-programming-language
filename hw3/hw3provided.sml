(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)


fun only_capitals xs =
    List.filter (fn x => Char.isUpper (String.sub (x, 0))) xs

fun longest_string1 xs =
    List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc )  ""  xs

fun longest_string2 xs =
    List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc )  ""  xs

fun longest_string_helper f xs =
    List.foldl (fn (x, acc) => if f (String.size x, String.size acc) then x else acc) "" xs
	       
val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >=y)
					    
fun longest_capitalized xs =
    (longest_string3 o only_capitals) xs


fun rev_string str =
    (String.implode o List.rev o String.explode ) str

						  
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs'  => case f x of
		       NONE => first_answer f xs'
		     | SOME v => v
				     
fun all_answers f xs =
    let	fun all_answers_helper f xs acc   =
	    case xs of
		[] => SOME acc
	      | x::xs' => case  f x of
			      NONE => NONE
			    | SOME lst => all_answers_helper f xs' (lst@acc) 
    in	all_answers_helper f xs []    end
	

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu
					    

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

val count_wildcards = g (fn () => 1) (fn x => 0) 
val count_wild_and_variable_lengths = g (fn () => 1) String.size
fun  count_some_var (str, pattern) =
     g (fn () => 0 ) (fn x => if x = str then 1 else 0) pattern

fun check_pat p =
    let
	fun get_variable_list p =
	    case p of
		Variable x =>  [x]
	      | TupleP ps  => List.foldl (fn (p, acc) => (get_variable_list p)@acc) [] ps
	      | ConstructorP (_,p)  => get_variable_list p
	      | _ => []
	fun string_list_distinct strList =
	    case strList of
		[] => true
	      | x::xs' => not (List.exists (fn str=> str = x ) xs')
			      andalso string_list_distinct xs'
    in
	(string_list_distinct o get_variable_list) p
    end

fun match (valu, pattern) =
    case (valu, pattern) of
	(_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP cp) => if c = cp then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then case (vs, ps) of
					  ([],[]) => SOME [] 
				       |  (v::vs, p::ps) => case (match (v, p), match (Tuple vs, TupleP ps)) of
								(NONE,_) => NONE
							      | (_,NONE) => NONE
							      | (SOME lst1, SOME lst2) => SOME (lst1 @ lst2)
				 else NONE
      | (Constructor (s2,v), ConstructorP (s1,p)) => if s1 = s2
						     then match(v, p) 
						     else NONE
      | _ => NONE

fun first_match value patternList  =
    SOME (first_answer (fn pattern =>match (value, pattern)) patternList)  handle NoAnswer => NONE
	
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


    
