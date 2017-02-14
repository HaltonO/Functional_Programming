
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

(* Description of g:
g is curried and takes 3 arguments: 2 functions f1 and f2 and a pattern p. 
f1 takes a unit, f2 takes a string and function g tests a given 
set of values of both f1 and f2. g result depends also on pattern p.

Wildcard: returns an empty list of bindings
Variable x: returns a single element list 
TupleP ps: returns a list
ConstructorP(_,p): returns a list of pattern matchs
_: returns 0
   
*)

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


(**** put all your code after this line ****)

(*function 1*)	
fun only_capitals(lst: string list): string list =
	List.filter(fn x => Char.isUpper(String.sub(x, 0))) lst

(*function 2*)		
fun longest_string1(lst: string list): string =
	List.foldl (fn (x,y) => 
	if String.size(x) > String.size(y) then
	x 
	else y) "" lst
	
(*function 3*)	
fun longest_string2(lst: string list): string =
	List.foldl (fn (x,y) => 
	if String.size(x) >= String.size(y) then
	x 
	else y) "" lst

(*function 4*)	
fun longest_string_helper(help: int * int -> bool) =
	fn lst: string list => 
	List.foldl (
	fn (x,y) => 
	if help(String.size(x),String.size(y)) then 
	x 
	else y
	) "" lst
	
val longest_string3 = longest_string_helper(fn (x,y) => x > y)
val longest_string4 = longest_string_helper(fn (x,y) => x >= y)

(*function 5*)	
fun longest_capitalized(str): string =
  (longest_string1 o only_capitals) str
				    
(*function 6*)
val rev_string = String.implode o List.rev o String.explode

(*function 7*)
fun first_answer f lst =
	case lst of
	[] => raise NoAnswer
	| head::tail =>
		case f head of
		NONE => first_answer f tail
		| SOME value => value
		
(*function 8*)
fun all_answers f lst =
    let
        fun answer f lst acc =
            case lst of
                [] => SOME acc
              | head::tail =>
                case f head of
                    NONE => NONE
                  | SOME y => answer f tail (acc@y)
    in
        answer f lst []
    end


(* function 9 b *)
fun count_wildcards(pattern)=	
	g (fn () => 1) (fn _ => 0) pattern
	
(* function 9 c *)
fun count_wild_and_variable_lengths(pattern)=	
    g (fn () => 1) (fn (y) => String.size(y)) pattern
	
(* function 9 d*)	
fun count_some_var(s, p) =
    g (fn x => 0) (fn x => if s=x then 1 else 0) p	

(* function 10 *)      
fun check_pat(pattern) =
    let
	fun str(pattern) =
	    case pattern of
		Variable x => [x]
	      | TupleP y => List.foldl (fn (pattern,b) => (str pattern)@b) [] y
	      | ConstructorP(_,pattern) => str pattern
	      | _ => []
	fun help(st) =
	    case st of
		[] => true
	      | head::tail =>
		if List.exists(fn x => x=head) tail then false else help tail
    in
	help(str(pattern))
    end


(* function 11 *)
fun match (value,pattern) =
    let
      fun tpl(value,pattern)=
          case (value,pattern) of
              (head1::tail1, head2::tail2) =>
	      (match(head1,head2))::tpl(tail1,tail2)
            | _ => []
    in
      case (pattern,value) of
	  (Wildcard,_) => SOME []
        | (Variable x,_) => SOME [(x, value)]
        | (UnitP, Unit) => SOME []
        | (ConstP n, Const ve) => if ve = n then SOME [] else NONE
        | (TupleP pat, Tuple vl) =>
          if List.length(vl) = List.length(pat) then (*compare lengths*)
            all_answers (fn x => x) (tpl(vl,pat))
          else
            NONE
        | (ConstructorP(st1,pt), Constructor(st2,va)) =>
          if (st1 = st2) then match(va,pt) else NONE
        |  _  => NONE (*| (_,_) => NONE*)
    end	

(* function 12 *)	
fun first_match value pattern =
  SOME (first_answer (fn p => match(value, p)) pattern)
  handle NoAnswer => NONE	
