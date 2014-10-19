(* type is int lit -> int *)
fun sum_list xs =
    case xs of
	[] => 0
      | x::xs' => x + sum_list xs' 

(* type is 'a list * 'a list -> 'a list  *)
fun append (xs, ys) =
    case xs of
	[] => ys
      | x::xs' => x :: append (xs', ys)

(* this really is /exactly/ how options are defined
careful : now shadowing the build in ones   *)
datatype 'a option = NONE | SOME of 'a

datatype 'a mylist = Empty | Cons of 'a * 'a mylist

datatype ('a,'b) tree = Node of 'a * ('a,'b) tree * ('a,'b) tree
		      | Leaf of 'b

(* type is (int,int) tree -> int  *)
fun sum_tree tr =
    case tr of
	Leaf i => i
     |  Node (i,lft,rgt)  =>  i + sum_tree lft + sum_tree rgt

(* type is ('a,int) tree -> int  *)
fun sum_leaves tr =
    case tr of
	Leaf i => i
     |  Node(i,lft,rgt)  => sum_leaves lft + sum_leaves rgt

(* type is ('a,'b) tree -> int  *)
fun num_leaves tr =
    case tr of     
	Leaf i => 1
      | Node(i,lft,rgt)  => num_leaves lft + num_leaves rgt 
		  
							    
