(* Coursera Programming Languages, Homework 3, Provided Code *)

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

fun only_capitals xs =
    List.filter
	(fn s =>
	    Char.isUpper(String.sub(s,0))
	)
	xs;



fun longest_string1 xs =
    List.foldl
	(fn (s, acc) =>
	    if String.size(s) > String.size(acc)
	    then s
	    else acc
	)
	""
	xs;


fun longest_string2 xs =
    List.foldl
	(fn (s, acc) =>
	    if String.size(s) >= String.size(acc)
	    then s
	    else acc
	)
	""
	xs;

fun longest_string_helper cmp_fn xs =
    List.foldl
	(fn(s, acc) =>
	    if cmp_fn(String.size(s), String.size(acc))
	    then s
	    else acc		       
	)
	""
	xs;
		
val longest_string3 = longest_string_helper(fn (x, y)=> x > y);

val longest_string4 = longest_string_helper(fn (x, y)=> x >= y);
    


			
val longest_capitalized = longest_string1 o only_capitals;


val rev_string = String.implode o List.rev o String.explode;

    
fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
      | x::xs' =>
	case f x of
		   SOME v => v
		 | NONE  => first_answer f xs';


fun all_answers f lst =
    let
	fun helper(acc, lst)=
	    case lst of
		[] => SOME acc
	      | x::xs' =>
		case f x of
		    SOME v => helper(v @ acc, xs')
		  | NONE => NONE
    in
	helper([], lst)
    end





	
(*count wild cards, since g calls f1 when match wild card, we make f1 1  *)



fun count_wildcards p =
    let
	fun f1 () = 1
	fun f2 _ = 0
    in
	g f1 f2 p
    end
	

fun count_wild_and_variable_lengths p =
    let
	fun f1 () = 1
	fun f2 s = String.size(s)
    in
	g f1 f2 p
    end
	
	

fun count_some_var (s_name, p) =
    let
	fun f1 () = 0
	fun f2 s = if s = s_name then 1 else 0
	    
    in g f1 f2 p
    end
	
    
fun check_pat p=
    let
	fun get_vars p =
	    case p of
		Variable x => [x]
	      | TupleP ps  => List.foldl(fn(p , acc) => acc @ get_vars(p)) [] ps
	      | ConstructorP(_,p) => get_vars(p)
	      | _ => []
			 
	fun no_dup lst =
	    case lst of
		[] => true
	      | x::xs' =>
		if List.exists(fn x' => x'=x) xs' then false
		else no_dup xs'
    in
	(no_dup o get_vars) p	    
				
    end
	
	    
(*TupleP is pattern list*)
	
fun match (v, p) =
    case (v, p) of
	 (_, Wildcard) => SOME []
       | (_, Variable s) => SOME[(s, v)]
       | (Unit, UnitP) => SOME[]
       | (Const x1, ConstP x2) => if x1 = x2 then SOME [] else NONE
       | (Tuple vs, TupleP ps) =>
	 if List.length(vs) = List.length(ps) then
	     all_answers match (ListPair.zip(vs, ps))
	 else NONE
       | (Constructor (s1, v), ConstructorP (s2, p)) =>
	 if s1 = s2 then match(v, p) else NONE
       |_  => NONE
	

							     
fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps) handle NoAnswer => NONE



									 
