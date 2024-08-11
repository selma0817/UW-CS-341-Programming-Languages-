
(*take 2 dates and evalute true or false depending on if first date comes before the second date*)
fun is_older(date1 : int*int*int, date2: int*int*int) =
    if (#1 date1 <  #1 date2) then true
    else if(#1 date1 >  #1 date2) then false
    else if(#2 date1 <  #2  date2) then true
    else if(#2 date1 >  #2  date2) then false
    else if(#3 date1 <  #3 date2) then true
    else false
					   
(*takes a list of dates and a month, return how many number in dates is in given month*)
(*i cannot use a counter because mutation is not allowed*)
fun number_in_month(dates : (int*int*int) list, month: int)=
    if null dates
    then 0
    else if  #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)
			    
(*takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in
the list of dates that are in any of the months in the list of months.*)
fun number_in_months(dates: (int*int*int) list, months: int list)=
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
			
fun dates_in_month(dates:  (int*int*int) list, month: int)=
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

		       
fun dates_in_months(dates: (int*int*int) list, months: int list)=
    if null months then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

							   
fun get_nth(s : string list, n: int)=
    if n=1  then hd s
    else get_nth(tl s, n-1)
			     
			     
fun date_to_string(date : (int*int*int))=
    let
	val months  = ["January", "February","March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	val month_name = get_nth(months, #2 date)
    in
	month_name ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, l: int list)=
    let fun count(cur_sum: int, n: int, l: int list) =
	    if(cur_sum + hd l >=  sum) then n
	    else count(cur_sum + hd l, n+1, tl l)
    in
	count(0, 0, l)
    end

	
fun what_month(day: int)=
    let
	val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, months)+1
    end

fun month_range(day1 : int, day2 : int)=
    if day1 > day2 then []
    else
	what_month(day1)::month_range(day1+1, day2)

(*essentially asking to find min date. use is_older here *)		     
fun oldest(dates: (int*int*int) list)=
    if null dates
    then NONE
    else let
	     fun oldest_nonempty(dates: (int*int*int) list)=
		 if null (tl dates)
		 then hd dates
		 else let val old = oldest_nonempty(tl dates)
		      in
			  if is_older(hd dates, old)
			  then hd dates
			  else old
		      end
	     
	 in
	     SOME(oldest_nonempty(dates))
	 end
	     
	
	
    
    
