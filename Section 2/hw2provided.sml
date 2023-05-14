(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*Exercise 1*)

(*1A*)
fun all_except_option (str, strs) =
   let fun aux (str,strs,acc) =
         case strs of
            [] => NONE
            |s::ss' => if same_string(s,str) then SOME (acc@ss') else aux(str,ss',s::acc)
   in
      aux(str,strs,[])
   end

val test1A_1 = all_except_option ("string", ["string"]) = SOME []  
val test1A_2 = all_except_option ("apple", ["apple","banana","grapes","pinapple"]) = SOME ["banana","grapes","pinapple"]

(*1B*)

fun get_substitutions1 (substitutions, s) =
   case substitutions of
      [] => []
      |sub::subs => (case sub of
                        [] => []
                        |_::ss => (case all_except_option(s,sub) of
                                    NONE => get_substitutions1(subs,s)
                                    |SOME v => v@get_substitutions1(subs,s)))

val test1B_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test1B_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") =  ["Fredrick","Freddie","F"]
val test1B_3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

(*1C*)
fun get_substitutions2 (substitutions, s) =
   let fun aux(substitutions, s, acc) =
      case substitutions of
         [] => acc
         |sub::subs => (case all_except_option(s,sub) of
                           NONE => aux(subs,s,acc)
                           |SOME v => aux(subs,s,acc@v))
   in
      aux(substitutions,s,[])
   end

val test1C_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test1C_2 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") =  ["Fredrick","Freddie","F"]
val test1C_3 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

(*1D*)
fun similar_names(substitutions, {first=f,middle=m,last=l}) =
   let val subs = get_substitutions2(substitutions,f) 
   in
      let fun aux (subs, {first=f,middle=m,last=l}, acc) =
         case subs of
            [] => acc
            |s::ss => aux(ss,{first=f,middle=m,last=l},{first=s,middle=m,last=l}::acc)
      in
         aux(subs, {first=f,middle=m,last=l}, [{first=f,middle=m,last=l}])
      end
   end

val test1D_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="F", last="Smith", middle="W"},{first="Freddie", last="Smith", middle="W"},
       {first="Fredrick", last="Smith", middle="W"},{first="Fred", last="Smith", middle="W"}]


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*Exercise 2*)

(*2A*)

fun card_color c =
   case c of
      (suit,rank) => if suit = Diamonds orelse suit = Hearts then Red else Black

val test2A_1 = card_color (Clubs, Num 2) = Black

(*2B*)
fun card_value (suit,rank) =
   case rank of
      Ace => 11
      |Num n => n 
      |_ => 10

val test2B_1 = card_value (Clubs, Num 2) = 2
val test2B_2 = card_value (Clubs, Ace) = 11
val test2B_3 = card_value (Clubs, Queen) = 10

(*2C*)
fun remove_card (cs,c,e) =

   let fun aux (cs,c,e,acc) =
         case cs of
            [] => raise e 
            |card::cards => if card = c then acc@cards else aux(cards,c,e,card::acc)
   in
      aux(cs,c,e,[])
   end

val test2C_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
(*val test2C_2 = remove_card ([(Hearts, Ace)], (Clubs, Ace), IllegalMove)*)

(*2D*)
fun all_same_color cs =
   case cs of
      [] => true
      |c::[] => true
      |c1::c2::cs' => card_color(c1) = card_color(c2) andalso all_same_color(cs')

val test2D_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test2D_2 = all_same_color [(Hearts, Ace), (Clubs, Ace),(Diamonds, Ace)] = false
val test2D_3 = all_same_color [(Hearts, Ace), (Diamonds, Ace),(Diamonds, Ace)] = true

(*2E*)
fun sum_cards cards =
   let fun aux (cards, acc) =
      case cards of
         [] => acc
         |c::cs => aux(cs,card_value(c) + acc)
   in
      aux (cards,0)
   end

val test2E_1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test2E_2 = sum_cards [] = 0

(*2F*)
fun score (cards,goal) =
   let val sum = sum_cards(cards)
      val same_color = if all_same_color(cards) then 2 else 1
   in
      if sum>goal then 3*(sum-goal) div same_color else (goal-sum) div same_color
   end

val test2F_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test2F_2 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test2F_3 = score ([(Hearts, Ace),(Diamonds, Num 4)],10) = 7

(*2G*)
fun officiate(cards,moves,goal) =
   let fun aux (cards,moves,goal,hand) =
      case moves of
         [] => score (hand,goal)
         |m::ms => 
            (case m of
               Draw =>  
                  (case cards of 
                     [] => score (hand,goal)
                     |c::cs => if sum_cards(hand) > goal
                              then  score (hand,goal)
                              else aux(cs,ms,goal,c::hand))
               |Discard c => aux(cards,ms,goal,remove_card(hand,c,IllegalMove)))
   in
      aux(cards,moves,goal,[])
   end

val test2G_1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test2G_2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test2G_3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)