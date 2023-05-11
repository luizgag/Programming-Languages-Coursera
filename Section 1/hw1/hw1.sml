
(* Exercise 1 *)
fun is_older (date1:(int*int*int),date2:(int*int*int)) =
    let
      val y1 = #1 date1
      val m1 = #2 date1
      val d1 = #3 date1

      val y2 = #1 date2
      val m2 = #2 date2
      val d2 = #3 date2
    in
      if y1 < y2 then true else 
        if y1 > y2 then false else 
            if m1 < m2 then true else
                if m1 > m2 then false else
                    if d1 < d2 then true else false
    end

val test11 = is_older ((1,2,3),(2,3,4)) = true
val test12 = is_older((1,1,2),(1,1,3)) = true
val test13 = is_older((1,1,2),(1,1,2)) = false
val test14 = is_older((1,1,2),(1,1,1)) = false
val test15 = is_older((1,2,2),(1,3,1)) = true
val test16 = is_older((1,4,2),(1,3,1)) = false
val test17 = is_older((1,4,2),(2,3,1)) = true
val test18 = is_older((3,4,2),(2,3,1)) = false

(* Exercise 2 *)
fun number_in_month (dates: (int*int*int) list, month: int) =

    if null dates
    then 0
    else if #2 (hd dates) = month then 1 + number_in_month(tl dates,month) else 0 + number_in_month(tl dates,month)

val test21 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test22 = number_in_month ([],2) = 0
val test23 = number_in_month ([(2012,5,28),(2013,12,1),(2012,5,28),(2013,12,1)],2) = 0
val test24 = number_in_month ([(2012,5,28),(2013,12,1),(2012,5,28),(2013,12,1)],5) = 2

(* Exercise 3 *)
fun number_in_months (dates: (int*int*int) list, months: int list) =

    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

val test31 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

(* Exercise 4 *)
fun dates_in_month(dates:(int*int*int) list, month:int) =
        if null dates
        then []
        else if #2(hd dates) = month
            then hd dates::dates_in_month(tl dates, month) 
            else dates_in_month(tl dates, month)

val test41 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

(* Exercise 5 *)

fun dates_in_months (dates:(int*int*int) list, months:int list) =
    if null months
    then []
    else dates_in_month(dates,hd months) @ dates_in_months(dates,tl months)

val test51 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]    

(* Exercise 6 *)
fun get_nth (strs: string list, n : int) =

    if n = 1
    then hd strs
    else get_nth(tl strs, n-1)

val test61 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

(* Exercise 7 *)

fun date_to_string (date: (int*int*int)) =
    let
        val months = ["January", "February", "March", "April", "May", 
                        "June", "July", "August", "September", "October",
                        "November", "December"]
        
    in
        get_nth(months, #2 date) ^ " " ^Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

val test71 = date_to_string (2013, 6, 1) = "June 1, 2013"

(* Exercise 8 *)

fun number_before_reaching_sum(sum: int, xs: int list) =

    if  hd xs >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd(xs), tl(xs))

val test81 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test82 = number_before_reaching_sum (25, [5,5,5,5,5,5,5,5]) = 4

(* Exercise 9 *) 
fun what_month(day: int) =
    let 
        val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day,days) + 1
    end

val test91 = what_month 70 = 3
val test91 = what_month 60 = 3

(* Exercise 10 *)
fun month_range (day1: int, day2:int) =

    if day2 < day1
    then []
    else what_month(day1)::month_range(day1+1,day2)

val test101 = month_range (31, 34) = [1,2,2,2]
val test102 = month_range (30, 34) = [1,1,2,2,2]

(* Exercise 11 *)

fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else 
        let 
            fun oldest_nonempty (dates: (int*int*int) list) =
                if null (tl dates) 
                then hd dates
                else 
                    let val tl_ans = oldest_nonempty(tl dates)
                    in
                        if is_older(hd dates, tl_ans)
                        then hd dates
                        else tl_ans
                    end
        in
            SOME (oldest_nonempty dates)
        end
       
val test111 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)