(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* func 1*)
fun all_except_option(s, lst)=
    let
	fun remove_string(lst, acc)=
	    case lst of
		[] => NONE
	      | x::xs =>  
		     if same_string(s, x) then SOME(acc @ xs)
		     else remove_string(xs, acc@[x])
    in
	remove_string(lst, [])
    end


(* func 2*)
fun get_substitutions1(sll, s)=
    case sll of
	[]=> []
     |  x::xs =>
	case all_except_option(s, x) of
		 NONE =>  get_substitutions1(xs, s)
	       | SOME lst => lst @  get_substitutions1(xs, s)

(*func 3*)
fun get_substitutions2(sll, s)=
    let
	fun helper(sll, acc) =
	    case sll of
		[] => acc
	      | x::xs  =>
		case all_except_option(s, x) of
		    NONE=> helper(xs, acc)
		  | SOME lst => helper(xs, lst @ acc) 
		   
    in
	helper(sll ,[])
	      
    end

(* func 4*)
fun similar_names(sll, name)=
    let
	val {first, middle, last} = name
	val subs =  get_substitutions1(sll, first)
	fun helper(subs, acc)=
	    case subs of
		[] => acc
	       |x::xs => helper(xs, acc @ [{first=x, middle=middle, last=last}])
    in
	helper(subs, [name])
    end



	
(*card game*)

fun card_color card =
    case card of
	(Clubs, _) => Black
      | (Spades,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red

	

				     
				     
fun card_value card =
    case card of
	(_,Ace) => 11
      | (_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Num n)  => n


			  

fun remove_card(cs, c, e)=
    let
	fun helper(cs, acc)=
	case cs of
	    [] => raise  e
	  | c'::cs' =>
	    if c' = c then acc @ cs'
	    else helper(cs', acc@[c'])
    in
	helper(cs, [])
    end
	

		       
fun all_same_color cards=
    case cards of
	[]=> true
      | _::[] => true
      | card1::(card2::rest) =>  (card_color(card1)=card_color(card2) andalso all_same_color(card2::rest))

		  
fun sum_cards cards=
    let
	fun helper(crds, acc)=
	    case crds of
		[]=> acc
	      | c::crds' => helper(crds', acc + card_value(c))
    in
	helper(cards, 0)
    end

	
	
fun score(held_cards, goal)=
    let
	val sum = sum_cards(held_cards)
	val p_score = if sum > goal then 3*(sum - goal) else (goal-sum)
	val final_score = if all_same_color held_cards then p_score div 2 else p_score						
			      
    in
	final_score
	
    end


		
	
    
fun officiate(card_list, moves, goal)=
    let
	fun  helper(card_list, held_cards, moves)=
	     case moves of
		 []=> score(held_cards, goal)
		| Draw::rest_moves =>
		  (case card_list of
		      [] => score(held_cards, goal)
		    | drawn_c :: rest_card_list =>
		      let
			  val new_held_cards = drawn_c :: held_cards
			  val new_sum = sum_cards(new_held_cards)
		      in
			  if new_sum > goal then score(new_held_cards, goal)
			  else helper(rest_card_list, new_held_cards, rest_moves)
		      end)	  
		| Discard card_to_discard :: rest_moves =>
		  let
		      val new_held_cards = remove_card(held_cards,  card_to_discard,  IllegalMove)
		  in
		      helper(card_list, new_held_cards, rest_moves)
		  end		      		  
	    
    in
	helper(card_list, [], moves)
    end
	
			     



