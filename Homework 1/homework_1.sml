(*function number #1*)
fun is_older(date_one: int * int * int, date_two: int * int * int ) = 
    (365 * #1 date_one + 31 *  #2 date_one + #3 date_one) - (365 * #1 date_two + 31 *  #2 date_two + #3 date_two) < 0   

(*function number #2*)
fun number_in_month (m_list : (int * int * int) list, num : int) = 
    if null m_list
    then 0
    else if #2 (hd m_list) = num
    then 1 + number_in_month(tl m_list, num)
    else number_in_month(tl m_list, num)

(*function number #3*)
fun number_in_months (d_list : (int * int * int) list, m_list : int list) =
    if null m_list
    then 0
    else number_in_month(d_list,hd m_list) + number_in_months(d_list, tl m_list)

(*function number #4*)
fun dates_in_month (d_list : (int * int * int) list, month : int) =
    if null d_list
    then []
    else if month = #2 (hd d_list)
    then hd d_list :: dates_in_month(tl d_list, month)
    else dates_in_month(tl d_list, month)

(*function number #5*)
fun dates_in_months (d_list : (int * int * int) list, m_list : int list) =
    if null m_list
    then []
    else dates_in_month(d_list, hd m_list) @ dates_in_months(d_list, tl m_list) 

(*function number #6*)
fun get_nth (p: string list, i : int) =
    if i = 1
    then hd p 
    else get_nth(tl p,i-1)

(*function number #7*)
fun date_to_string (d : int * int * int) =
    let  
	val month = ["January","February","March", "April", "May", "June", "July", "August", "September", "October", "November","December"]
    in 
	get_nth(month, #2 d) ^ " " ^ Int.toString (#3 d) ^ ", " ^ Int.toString (#1 d)
    end

(*function number #8
fun number_before_reaching_sum (number: int, mainlist : int list ) = 
    let 
	fun inc (x : int) = x + 1
	fun get_nth_sum (lst : int list, n : int) = (*sums the elements from the first to the n element of the list*)
	    if n = 1
	    then hd lst
	    else hd lst + get_nth_sum(tl lst, n-1)
	fun bigger_one (lst : int list, number : int, cont : int) = (*counts until finds a sum in the list that is bigger than the number*)
	    if get_nth_sum(lst, cont + 1) >= number
	    then cont
	    else bigger_one(lst, number, inc cont)
	fun get_nth_num (lst : int list, n : int) = (*gets the n element of the list*) 
	    if n = 1
	    then hd lst
	    else get_nth_num(tl lst, n-1)
    in
	get_nth_num(mainlist, bigger_one(mainlist, number, 1))
    end
*)
fun number_before_reaching_sum (number: int, mainlist : int list) =
    if null mainlist
    then 0
    else if number > hd mainlist
    then 1 + number_before_reaching_sum (number - hd mainlist, tl mainlist)
    else 0

val test = number_before_reaching_sum(10,[1,2,1,5,6])

(*function number #9*)
fun what_month (days : int) =
    let	
	val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31] 
	fun month_num (lt : int list, n : int, cont : int) =
	    if null lt
	    then cont
	    else if (n - hd lt) <= 0
	    then cont
	    else month_num(tl lt, n - hd lt,  cont + 1)
    in
	month_num(days_in_months, days, 1)
    end

(*function number #10*) 
fun month_range (day1 : int, day2 : int) = 
    if day1  = day2 + 1 
    then []
    else what_month(day1) :: month_range(day1 + 1,day2)

(*function number #11*)
fun oldest (d_list : (int * int * int) list) =
    if null d_list
    then NONE
    else let 
	    fun oldest_one (d_list : (int * int * int) list) =
		if null (tl d_list)  
		then hd d_list
		else let val tl_ans = oldest_one(tl d_list)
		     in
			 if is_older(hd d_list, tl_ans)
			 then hd d_list
			 else tl_ans
		     end
	in
	    SOME (oldest_one d_list)
	end
