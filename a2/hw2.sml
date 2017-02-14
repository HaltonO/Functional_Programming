
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)
   
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*1*)
fun all_except_option(str1: string, str2: string list)=
	case str2 of
	[] => NONE
	| head::tail => 
	if(same_string(head, str1))
		then SOME tail
	else
		case all_except_option(str1, tail) of 
		NONE  => NONE
		 | SOME  lis => SOME (head::lis)
		 
(*2*)
fun get_substitutions1(str: string list list, s: string): string list =
	case str of
	[] => []
	| head::tail =>
		case all_except_option(s,head) of 
		NONE => get_substitutions1(tail, s)		  
		| SOME lis => lis @ get_substitutions1(tail, s) 
		
(*3*)
fun get_substitutions2(str: string list list, s: string): string list =
	let fun helper(str: string list list, s: string, result: string list): string list =
		case str of
		[] => result
		| head::tail =>
			case all_except_option(s,head) of 
			NONE => helper(tail, s, result)		  
			| SOME lis => helper(tail, s, result@lis) 
	in
		helper(str, s, [])
	end
	
(*4*)
fun similar_names(str: string list list, {first=f,middle=m,last=l})=
     let fun helper(str: string list)=
	   case str of
	    [] => []
	    | head::tail => {first=head, middle=m, last=l}::helper(tail)
	 in
	 helper(f::get_substitutions1(str, f))
	 end

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(*5*)
fun card_color(name: card): color =
	case name of
	(suit, rank) => if(suit = Clubs orelse suit = Spades)
		then Black
	else
		Red
	
(*6*)
fun card_value(name: card): int = 
	case name of
	(_, Ace) => 11
	|(_, Num i) => i
	|(_,_) => 10
	
(*7*)	
fun remove_card(cs: card list, c: card, e): card list = 
	case cs of
	[] => raise e
	| head::tail => if head = c then tail 
	else head::remove_card(tail, c, e)

(*8*)
fun all_same_color(cs) =
	case cs of
		[] => true
	  | x::[] => true
	  | x::y::xs' => (card_color(x) = card_color(y)) andalso all_same_color(y::xs')
	
(*9*)
fun sum_cards(cs: card list): int =
	(*case cs of
	[] => 0
	| head::tail => card_value(head) + sum_cards(tail)*)
	let fun help(cs: card list, count): int =
	case cs of
		[] => count
		| head::tail => help(tail, card_value(head)+count)
	in
		help(cs, 0)
	end

(*10*)

fun score(cs: card list, goal: int): int = 	
	let
	fun helper(cs: card list, goal: int): int =
		if all_same_color(cs) then 
			(if sum_cards(cs)>goal then 
				sum_cards(cs)-goal 
				else 
					(goal-sum_cards(cs)) div 2)
		else if (sum_cards(cs)) > goal then 
			(2 * (sum_cards(cs)-goal))
			else if (sum_cards(cs)) < goal then 
				(goal-sum_cards(cs)) 
				else 0
	in
	helper(cs, goal)
	end
	
(*11*)
fun officiate(cs: card list, m: move list, goal: int): int =	
	let
		fun helper(cs: card list, m: move list, goal: int, held: card list, e)=
		case m of
		[] => score(held, goal)
		| head::tail => if(sum_cards(held)>goal) then score(held,goal) 
		else
			case head of
			Discard card => helper(cs, tail, goal, remove_card(held, card, e),e)
			| Draw => 
				case cs of
				[] => score(held, goal)
				| hd::tl => helper(tl, tail, goal, hd::held, e)
	in
	helper(cs, m,goal, [], IllegalMove)
	      end

		  
(*******************************************************)
exception notFound
	    
val test1_0 = all_except_option("2", ["3", "1", "2"]) = SOME ["3", "1"]
val test2_0 = get_substitutions1([["kal", "kelly"], ["joe", "james", "kal"]], "kal") = ["kelly", "joe", "james"]
val test3_0 = get_substitutions2([["sara", "susan"],["susan", "maddy", "lilly"]], "susan") = ["sara", "maddy", "lilly"]
val test4_0 = similar_names([
                             ["sara", "susan"],
                             ["sharon", "alexa","jenn"],
                             ["sara", "sue"]
                         ], {first="sara", middle = "(sam)", last="james"}) =
            [{first="sara",last="james",middle="(sam)"},
             {first="susan",last="james",middle="(sam)"},
             {first="sue",last="james",middle="(sam)"}]

val test5_0 = card_color(Clubs, Jack) = Black
val test6_0 = card_value(Clubs, Ace) = 11
val test7_0 = remove_card([(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 4), (Clubs, Num 4)], (Clubs, Ace), notFound) = [(Diamonds, Num 10), (Spades, Num 4), (Clubs, Num 4)]	
val test8_0 = all_same_color([(Diamonds,Num 10),(Spades,Num 5),(Clubs,Num 9)]) = false
val test9_0 = sum_cards([(Diamonds,Num 10),(Spades,Num 5),(Clubs,Num 9)]) = 24
val test10_0 = score([(Clubs, Ace)], 11) = 0
val test11_0 = officiate([(Diamonds,Num 10),(Spades,Num 5),(Clubs,Num 9)], [], 10) = 5	
