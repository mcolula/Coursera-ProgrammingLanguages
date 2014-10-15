type date = int * int * int

fun day (d : date) = #3 d

fun year (d : date) = #1 d

fun month (d : date) = #2 d

fun is_older (d1 : date, d2 : date) = 
    let 
	fun date_value(d : date) = year(d) * 365 + month(d) * 30 + day(d)
    in
	date_value(d1) < date_value(d2)
    end
  
fun number_in_month (dates : date list, m : int) = 
    if null dates
    then 0
    else 
	if month (hd dates) = m
	then number_in_month(tl dates, m) + 1
	else number_in_month(tl dates, m)

fun number_in_months (dates : date list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month (dates : date list, m : int) = 
    if null dates
    then []
    else
	let 
	    val d = hd dates
	in
	    if month (d) = m
	    then d :: dates_in_month(tl dates, m)
	    else dates_in_month(tl dates, m)
	end

fun dates_in_months (dates : date list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strings , n : int) = 
    if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)

fun date_to_string (d : date) = 
    let 
	val months = ["January", "February", "March", "April", "May", "June", 
                     "July", "Augoust", "September", "October", "November", "December"]
    in
	get_nth(months, month(d)) ^ " " ^ Int.toString(day(d)) ^ ", " ^ Int.toString(year(d))
    end

fun number_before_reaching_sum (sum : int, numbers : int list) = 
    if hd numbers >= sum
    then 0
    else 1 + number_before_reaching_sum(sum  - (hd numbers), tl numbers)
 
fun what_month (d : int) = 
    let 
	val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    in
	number_before_reaching_sum(d, days) + 1
    end

fun month_range (d1 : int, d2 : int) = 
    if d1 > d2
    then []
    else what_month(d1) :: month_range(d1 + 1, d2)

fun oldest (dates : date list) = 
    if null dates 
    then NONE
    else if null (tl dates)
    then SOME (hd dates)    else
	let 
	    val old = oldest(tl dates)
	in
	    if is_older(hd dates, Option.valOf(old))
	    then SOME (hd dates)
	    else old
	end

fun remove (nums : int list, repeated: int list) = 
    if null repeated
    then nums
    else
	let
	    fun contains (number : int, numbers : int list) =
		if null numbers
		then false
		else 
		    if number = hd numbers
		    then true
		    else contains(number, tl numbers)
	in 
	   if contains(hd repeated, nums)
	   then remove(nums, tl repeated)
	   else remove (nums @ [hd repeated], tl repeated) 
	end

fun number_in_months_challenge (dates : date list, months : int list) =
    number_in_months(dates, remove([], months))

fun dates_in_months_challenge (dates : date list, months : int list) =
    dates_in_months(dates, remove([], months))

fun reasonable_date (d : date) = 
    let 
	val leap_days   = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val usual_days  = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val leap_month  = get_nth(leap_days, month(d)) 
	val usual_month = get_nth(usual_days, month(d))
 	fun leap(y : int) = (y mod 400 = 0) orelse ((y mod 4 = 0) andalso (y mod 100 <> 0))
    in
	if leap(year(d))
	then (year(d) > 0) andalso (month(d) > 0) andalso (month(d) < 13) andalso (day(d) > 0) andalso (day(d) <= leap_month) 
	else (year(d) > 0) andalso (month(d) > 0) andalso (month(d) < 13) andalso (day(d) > 0) andalso (day(d) <= usual_month)
    end
