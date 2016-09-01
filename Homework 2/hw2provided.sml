
fun same_string(s1 : string, s2 : string) =
    s1 = s2


fun all_except_option (s, sl) =
    let fun is_there (s,sl) =  case sl of []=>false 
				     | c::sl' => if same_string(s,c) then true else is_there(s,sl')
    in case (s , sl) of
	    (s , sl) => let fun aux (a, b) = case (a,  b) of  
						 (_, []) => []
					       | (a,  b::bs) => if same_string(a, b) 
								then aux (a, bs)
								else b::aux(a,bs)
			in
			    if is_there(s,sl)
			    then SOME (aux(s, sl))
			    else NONE
			end 
    end

fun get_substitutions1 (sl,s) =
    case sl of
	[] => []
       |c::sl'=> case all_except_option(s,c) of
		     NONE => get_substitutions1 (sl',s)
		    |SOME i => i @ get_substitutions1 (sl',s)
						      
fun get_substitutions2 (sl, s) = 
    let
	fun helper (sl, s, acc) =
	    case sl of 
		[] => acc
	     |  c::sl' => case all_except_option(s,c) of 
			      NONE => helper(sl', s, acc)
			    | SOME i => helper(sl', s, acc@i)
    in 
	helper(sl,s,[])
    end

fun similar_names (sl,{first: string, middle: string, last: string}) =
    let val subs = get_substitutions2(sl, first) in
	case subs of 
	    []   => []
	 |  subs => let fun create_list (a,{first:string,middle:string,last:string}) =
			    case a of 
				[] => [] 
			     |  c::cs => ({first=c,middle=middle,last=last})::create_list(cs,{first=first,middle=middle,last=last})
		    in
			({first=first,middle=middle,last=last})::create_list(subs,{first=first,middle=middle,last=last})
		    end      	
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


fun card_color (crd, smthg) = 
    case crd of
	 Spades => Black 
      |  Clubs  => Black
      |  _      => Red

fun card_value (crd, vl) = 
    case vl of
         Num i => i
      |  Ace   => 11
      |  _     => 10 

fun remove_card (cs,c,e) = 
    case cs of
	[]=> raise e
	  | d::cs'=> case c=d of
			 true  => cs' 
		      |  false =>  d::remove_card(cs',c,e)  


fun all_same_color (cl) =
    case cl of
	[]=> true
      | c::[] =>true 
      |c::(c'::cl')=> if card_color(c) = card_color(c') 
		      then true andalso all_same_color((c'::cl'))
		      else false 

fun sum_cards (cl) =
    let fun sum (cl,con) = 
	    case cl of
		[]=>con
	       |c::cl' => sum(cl',con + card_value c) 
    in
	sum(cl,0)
    end 

fun score (cs, tr) =
    case  (cs, tr) of
          ([], tr) => 0
       |  (cs, tr) => let val cds = sum_cards cs val same_color = all_same_color(cs) in
			  if cds > tr
			  then if same_color
			       then 3 * (cds - tr) div 2
			       else 3 * (cds - tr)
			  else if same_color
			  then (tr - cds) div 2
			  else (tr - cds)
		      end 

fun officiate (cards, moves, goal) =
    let fun off (cards, moves, goal, held) =
	case (cards, moves, goal, held) of
	    (_ , [], goal, held) => score(held,goal)
	  | ([], _ , goal, held) => score(held,goal)
          |  (cd::cds, moves, goal, held) => case moves of
						(Discard a)::ms =>(case remove_card (held, a, IllegalMove) of
							               _ => off (cds, ms, goal,held))
				             |  Draw::ms    => case cd::cds of 
 								   [] => score(held,goal)
								 | _  => case sum_cards(held) > goal of
									     true => score(held,goal)
									  |  false => off(cds, ms, goal,cd::held)
    in
	off(cards,moves,goal,[])
    end
