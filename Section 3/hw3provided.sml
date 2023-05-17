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

(*Exercise 1*)
fun only_capitals ss =
	List.filter (Char.isUpper o (fn s => String.sub (s, 0))) ss

val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["Abc","bac","Caca"] = ["Abc","Caca"]

(*Exercise 2*)
fun longest_string1 ss =
	foldl (fn (acc, s) => if String.size acc > String.size s then acc else s) "" ss
	
val test2_1 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 ["A","bc","C","cd"] = "bc"

(*Exercise 3*)
fun longest_string2 ss =
	foldl (fn (acc, s) => if String.size acc >= String.size s then acc else s) "" ss
	
val test2_1 = longest_string2 ["A","bc","C"] = "bc"
val test2_2 = longest_string2 ["A","bc","C","cd"] = "cd"

(*Exercise 4*)

fun longest_string_helper f ss =
	foldl (f) "" ss

val longest_string3 = longest_string_helper (fn (acc, s) => if String.size acc > String.size s then acc else s)
val longest_string4 = longest_string_helper (fn (acc, s) => if String.size acc >= String.size s then acc else s)

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

(*Exercise 5*)
val longest_capitalized = longest_string1 o only_capitals

val test5 = longest_capitalized ["A","bc","C"] = "A"

(*Exercise 6*)

val rev_string = implode o List.rev o explode 

val test6 = rev_string "abc" = "cba"

(*Exercise 7*)

fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
		|x::xs' => (case f x of
					SOME v => v
					|NONE => first_answer f xs')


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

(*Exercise 8*)

fun all_answers f xs =
	let fun aux f xs acc =
		case xs of
			[] => SOME acc
			|x::xs' => (case f x of
							NONE => NONE
							|SOME v => aux f xs' (acc@v))
	in
		aux f xs []
	end

val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1] = SOME [1]

(*Exercise 9*)

(*9A*)
fun count_wildcards p =
	g (fn _ => 1) (fn s => 0) p

val test9a_1 = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (TupleP [Wildcard,Wildcard]) = 2

(*9B*)

fun count_wild_and_variable_lengths p =
	g (fn _ => 1) (fn s => String.size s) p

val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (TupleP [Variable("a"),Variable("banana"),Wildcard]) = 8

(*9C*)

fun count_some_var (str,p) =
	g (fn _ => 0) (fn s => if s = str then 1 else 0) p

val test9c_1 = count_some_var ("x", Variable("x")) = 1
val test9c_2 = count_some_var ("x", TupleP [Variable("x"),Wildcard,ConstructorP("banana",Variable("x"))]) = 2

(*Exercise 10*)

fun check_pat p =
	let
		fun pattern_to_strings p =
			case p of
			Wildcard          => []
			| Variable x        => [x]
			| TupleP ps         => List.foldl (fn (p,i) => i@(pattern_to_strings p)) [] ps
			| ConstructorP(_,p) => pattern_to_strings p
			| _                 => []

		fun has_repeats ss =
			case ss of
				[] => false
				|s::ss' => List.exists (fn x => x = s) ss' orelse has_repeats(ss')
	in
		not (has_repeats (pattern_to_strings p))
	end

val test10_1 = check_pat (Variable("x")) = true
val test10_2 = check_pat (TupleP [Variable("x"),Wildcard,ConstructorP("banana",Variable("x"))]) = false

(*Exercise 11*)

fun match (va,pat) =
	case (va,pat) of 
		(Unit,UnitP) => SOME []
		|(_,Wildcard) => SOME []
		|(v,Variable s) => SOME [(s,v)]
		|(Const i,ConstP j) => if i = j then SOME [] else NONE
		|(Tuple vs,TupleP ps) => all_answers match (ListPair.zip(vs,ps))
		|(Constructor(s1,v),ConstructorP (s2,p)) => if s1 = s2 then match(v,p) else NONE
		|_ => NONE

val test11_1 = match (Const(1), UnitP) = NONE
val test11_2 = match (Tuple [Const(1),Const(3),Constructor("banana",Const(2))], 
					  TupleP  [Variable("x"),Wildcard,ConstructorP("banana",Variable("y"))]
					 ) = SOME [("x", Const 1),("y", Const 2)]
val test11_3 = match(Unit,UnitP) = SOME []

(*Exercise 12*)

fun first_match v ps =
	(SOME (first_answer (fn p => match(v,p)) ps)) handle NoAnswer => NONE

val test12_1 = first_match Unit [UnitP] = SOME []
val test12_2 = first_match (Const 5) [Variable "X",Variable "Y"] = SOME [("X",Const 5)]