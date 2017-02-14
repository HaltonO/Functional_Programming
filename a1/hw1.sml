
type DATE = (int * int * int)
exception InvalidParameter

(*1*)	      
fun is_older(d1: DATE, d2: DATE): bool =
  if #1 d1 < #1 d2
	then true
  else if #1 d1 = #1 d2 andalso #2 d1 < #2 d2
	then true
  else if #1 d1 = #1 d2 andalso #2 d1 < #2 d2 andalso #3 d1 < #3 d2
	then true
  else
    false

(*2*)	  
fun number_in_month(date: DATE list, month: int): int =
  if null date
	then 0
  else if (#2 (hd date)) = month
	then 1 + number_in_month(tl date, month)
  else
    number_in_month(tl date, month)

(*3*)
fun number_in_months(date: DATE list, months: int list): int =
  if null months
	then 0
  else number_in_month(date, hd months) + number_in_months(date, tl months)

(*4*)								   
fun dates_in_month(date: DATE list, month: int): DATE list =
  if null date
	then []
  else if (#2 (hd date)) = month
	then (hd date)::dates_in_month(tl date, month)
  else
    dates_in_month(tl date, month)

(*5*)
fun dates_in_months(date: DATE list, months: int list): DATE list =
  if null months (* null date or null months??*)
	then []
  else
    dates_in_month(date, hd months)@dates_in_months(date, tl months)

(*6*)						     
fun get_nth(str: string list, n: int): string =
  if n <= 0 orelse null str
	then raise InvalidParameter
  else if n = 1
	then hd str
  else
    (*#n str*)
    get_nth(tl str, n - 1)

(*7*)
fun date_to_string(date: DATE): string =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
  end

(*8*)
fun number_before_reaching_sum(sum: int, lst: int list): int =
  if hd lst >= sum 
	then 0
  else
    1 +  number_before_reaching_sum(sum - ( hd lst),tl lst)

(*9*)
fun what_month(day: int): int =
  let 
	val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum(day, months)+1
  end
   
(*10*)
fun month_range(day1: int, day2: int): int list =
    if day1 > day2
	  then []
	else
	  what_month(day1)::month_range(day1 +1 , day2)

(*11*)
fun oldest(date: DATE list): DATE option =
    if null date
	  then NONE
	else if isSome(oldest(tl date)) andalso is_older(valOf(oldest(tl date)),hd date) 
	  then oldest(tl date)
	else
      SOME (hd date)

(*12*)
fun reasonable_date(d: DATE): bool =
    if(#2 d > 12 orelse #2 d < 1 orelse #1 d < 1)
       then false
    else
       let fun leap(d: DATE): bool =
	  if((#1 d mod 4) <> 0)
	      then false
	  else if(#1 d mod 100) <> 0 
	      then true
	  else if(#1 d mod 400) <> 0
	      then false
	  else true
       in
	  let fun m_days(d2: DATE): string list =
	    if(leap(d))
		    then ["31","29","31","30","31","30","31","31","30","31","30","31"]
	        else ["31","28","31","30","31","30","31","31","30","31","30","31"]
	    in
		if(#3 d <= valOf(Int.fromString(get_nth(m_days(d), #2 d))))
		    then true
		else false
	    end
	end
 (*   
     let
     fun leap(): bool =
     #1 date mod 400 = 0 orelse (#1 date mod 4 = 0 andalso #1 date mod 100 <>0)
     let  fun day(): bool =
		   in  if #2 date = 2 then
		     if leap()
		     then #3 date <=29 else #3 date <=28
		   else if #2 date = 1 orelse #2 date= 3 orelse #2 date =5 orelse #2date =7 orelse #2 date= 8 orelse #2 date = 										      orelse
	#2 date = 10 orelse #2 date = 12 then #3 date >=1 andalso #3 date <=31
		   else
		       #3 date >=1 andalso #3 date <=30
		end					 
	       in
		   if #1 date < 1 orelse #2 date < 1 orelse #2 date < 1
		   then false
		   else if 	#1 date > 0 andalso #2 date > 0 andalso #2 date < 13 andalso day()
				then true
		   
		       end
			   *)
		       
