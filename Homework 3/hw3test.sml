use "hw3test.sml";

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)


val only_capitals =  List.filter (fn x => Char.isUpper(String.sub(x, 0)))

val longest_string1 = foldl (fn (x,y) => if String.size x > String.size y then x else y) ""

val longest_string2 = foldl (fn (x,y) => if String.size x >= String.size y then x else y) ""

fun longest_string_helper f = foldl (fn (x,y) => if f(String.size x,String.size y) then x else y) ""

val longest_string3 = let val help = fn (x, y) => x > y in longest_string_helper help end

val longest_string4 = let val help = fn (x, y) => x >= y in longest_string_helper help end 

val longest_capitalized = longest_string1 o only_capitals
 
val rev_string  = implode o rev o explode 

fun first_answer f xs = if null xs 
			then raise NoAnswer
			else case f(hd xs) of 
				 SOME x => x
			      |  NONE   => first_answer f (tl xs) 
							
fun all_answers f lst = let fun helper f xs acc =  case xs of
						       [] => SOME acc
						   |  x::xs => case f x of
								   SOME x => helper f xs (x @ acc)
								|  NONE => NONE
			in
			    helper f lst []
			end

val count_wildcards  = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size(x))

fun count_some_var (s, p) = g (fn x => 0) (fn x => if s = x then 1 else 0) p

val check_pat  = let fun var (p: pattern) = case p of
						  Variable x => [x]
						| TupleP ps => List.foldl (fn (v,vs) => vs @ var(v)) [] ps
						| ConstructorP(_,p) => var(p)
						| _ => []
							   
		       fun unique xs = case xs of
					   [] => true
					|  x::xs' => not(List.exists (fn s => s = x) xs') andalso unique xs'
		   in
		       unique o var
		   end
													 
fun match (v: valu , p: pattern) = case p of
				        Variable x => SOME[(x, v)]
				     |  ConstructorP (a, b) => (case v of
								    Constructor(m, n) => if  a = m then match(n, b) else NONE
								 |  _ => NONE)
				     |  ConstP x => (case v of 
							 Const n => if n = x then SOME [] else NONE
						      |  _  => NONE)
				     |  TupleP xs => (case v of 
							 Tuple ts => if List.length xs = List.length ts
								     then all_answers match(ListPair.zip(ts, xs))
								     else NONE
							|_  => NONE) 
				     |  Wildcard => SOME [] 	
				     |  UnitP    => case v of 
							 Unit => SOME[]
						      |  _=> NONE

fun first_match v ps = SOME(first_answer(fn p => match(v, p)) ps) handle NoAnswer => NONE


val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a= longest_string3 ["A","bc","C"] = "bc"

val test4b= longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A";

val test6 = rev_string "abc" = "cba";

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1;

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

