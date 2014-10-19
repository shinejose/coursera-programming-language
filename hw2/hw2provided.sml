(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun list_reverse (xs, acc) =
    case xs of
	[] => acc
      | x::xs' => list_reverse (xs', x::acc) 
		
	      
fun all_except_option2 (str, xs) =
    let
	fun helper (str, xs, acc, is_in_list) =
	    case xs of
		[] => if is_in_list
		      then SOME (list_reverse (acc, []))
		      else NONE
	      | x::xs' => if same_string (x, str) 
			  then helper (str, xs', acc, true orelse is_in_list  )
			  else helper (str, xs', x::acc, false  orelse is_in_list  )
    in
	helper (str, xs, [], false ) 
    end

fun all_except_option (str, xs) =
    let
	fun helper (str, xs, is_in_list) = 
	    case xs of
		[] => if is_in_list
		      then SOME []
		      else NONE
	      | x::xs'  =>  if same_string (x, str)
			    then helper (str, xs', true orelse is_in_list)
			    else case helper (str, xs', false orelse is_in_list) of
				     NONE => NONE
				   | SOME y  =>  SOME (x :: y)
    in
	helper (str, xs, false) 
    end
	
fun get_substitutions1 (xs, str) =
    case xs of
	[] => []
      | x::xs'  => case all_except_option (str, x)  of
		       NONE => get_substitutions1 (xs', str) 
		     | SOME y => y @ get_substitutions1 (xs', str)

fun get_substitutions2 (xs, str) =
    let
	fun get_substitutions2_tail_recursive (xs, str, acc) =
	    case xs of 
		[] => acc
	      | x::xs'  =>  case all_except_option (str, x) of
			       NONE => get_substitutions2_tail_recursive (xs', str, acc)
			     | SOME y  => get_substitutions2_tail_recursive (xs', str, acc @ y) 
    in
	get_substitutions2_tail_recursive (xs, str, [])
    end
	
fun similar_names (xs, {first:string, middle:string, last:string} ) =
    let
	fun link_first_list xs =
	    case xs of
		[] => []
	      | x::xs' => {first=x, middle=middle, last=last} :: link_first_list xs'
    in	
	link_first_list ( first ::  get_substitutions2 (xs, first) ) 
    end		  


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color c =
    case c of
	(Spades,_) => Black
      | (Clubs,_)  => Black
      | (Hearts,_)  => Red
      | (Diamonds,_)  => Red


fun card_value (s, r) =
    case r of
	Num x => x
      | Ace  => 11
      | _ => 10

fun remove_card (cs, c, e) =
    let
	fun remove_card_helper (cs, c, e, isRemove) =
	    case cs of
		[] => if isRemove
		      then []
		      else raise e 
	      | x::cs' =>  if x = c andalso not isRemove
			   then remove_card_helper (cs', c, e, true orelse isRemove)
			   else x :: remove_card_helper (cs', c, e, false orelse isRemove) 
    in
	remove_card_helper (cs, c, e, false) 
    end


fun all_same_color cs =
    case cs of
	[] =>  true
      | x::[]  => true 
      | x::y::cs'  => card_color x = card_color y  andalso all_same_color (y::cs')

fun sum_cards cs =
    let
	fun sum_card_tail_recursive (cs, acc) =
	    case cs of
		[] => acc
	      | x::cs' => sum_card_tail_recursive (cs', card_value x + acc) 
    in
	sum_card_tail_recursive (cs, 0) 
    end

fun score (held_cards, goal) =
    let
	val sum_held_cards = sum_cards held_cards
	val preliminary_score = if sum_held_cards > goal
				then 3 * ( sum_held_cards - goal)
				else goal - sum_held_cards
    in
	if all_same_color held_cards
	then preliminary_score div 2
	else preliminary_score
    end

fun officiate (cardList, moveList, goal) =
    let
	fun play (cardList, heldCardList, moveList) =
	    case (cardList, heldCardList, moveList) of
		([],hc,_) => score (hc, goal)
	      | (_,hc,[]) => score (hc, goal)
	      | (c::cl', hc, m::ml') => case m of
					    Draw => if sum_cards (c::hc) > goal
						    then score (c::hc, goal)
						    else play (cl', c::hc, ml') 
					    | Discard c => play (c::cl', remove_card (hc, c, IllegalMove), ml')
    in
	play (cardList, [], moveList)
    end


fun score_challenge (held_cards, goal) =
    let
	val sum_held_cards = sum_cards held_cards
	val preliminary_score = if sum_held_cards > goal
				then 3 * ( sum_held_cards - goal)
				else goal - sum_held_cards
    in
	if all_same_color held_cards
	then preliminary_score div 2
	else preliminary_score
    end
	

	
	
	
	
					 
