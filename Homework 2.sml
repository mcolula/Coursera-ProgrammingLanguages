(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, sl) = 
    let 
	fun helper (left, done) =
	    case left of 
		[] => NONE
	      | x::xs => if same_string (x, str) 
			 then SOME (done @ xs) 
			 else helper (xs, done @ [x])
    in
	helper (sl, [])
    end

fun get_substitutions1 (sll, str) = 
    case sll of 
	[] => []
      | x::xs => case all_except_option (str, x) of 
		     NONE => get_substitutions1 (xs, str)
		   | SOME ls => ls @ get_substitutions1 (xs, str)

fun get_substitutions2 (sll, str) =
    let
	fun helper (sll, sl) = 
	    case sll of
		[] => sl
	      | x::xs => case all_except_option (str, x) of 
			     NONE => helper (xs, sl)
			   | SOME s => helper (xs,  sl @ s)
    in
	helper (sll, [])
    end


fun similar_names (subs, {first, middle, last}) = 
    let 
	fun helper (names, acc) = 
	    case names of 
		[] => acc
	      | x::xs => helper (xs, acc @ [{first = x, middle = middle, last = last}])
    in
	helper (first::get_substitutions1 (subs, first), [])
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
	(Spades, _) => Black
      | (Clubs, _) => Black
      | _ => Red

fun card_value c = 
    case c of 
	(_, Num i) => i
      | (_, Ace) => 11
      | _ => 10

 
fun remove_card (cs, c, e) =
    case cs of 
	[] => raise e
      | x::[] => if x = c then [] else raise e
      | x::xs => if x = c then xs else remove_card (xs, c, e)


fun all_same_color cs = 
    case cs of 
	[] => true
      | x::[] => true
      | x::y::xs => if card_color (x) = card_color (y)
		    then all_same_color (y::xs)
		    else false 

fun sum_cards cs = 
    let 
	fun helper (cs,  acc) =
	    case cs of 
		[] => acc 
	      | x::xs => helper (xs, acc + card_value (x))
    in
	helper(cs, 0)
    end
	    

fun score (cs, goal) = 
    let 
	val sum = sum_cards cs
	val pre = if sum > goal then 3 * (sum - goal) else goal - sum
    in
	case all_same_color cs of 
	    true  => pre div 2
	  | false => pre 
    end


fun officiate (cs, ms, goal) = 
    let 
	fun helper (held, cs, ms) = 
	    case ms of 
		[] => score (held, goal)
	      | (Discard c)::xs => helper(remove_card(held, c, IllegalMove), cs, xs)
	      | Draw::xs => case cs of
			    [] => score (held, goal)
			  | y::ys =>
			    let
				val sum = score (y::held, goal)
			    in 
				if sum > goal
				then score (y::held, goal)
				else helper(y::held, ys, xs)
			    end
    in
	helper([], cs, ms)
    end

