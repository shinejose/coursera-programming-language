fun is_older ( date1 : int * int * int  , date2 : int * int * int  ) =
    if (#1 date1)  < (#1 date2 )    then
	true 
    else if (#1 date1 ) > (#1 date2 ) then
	false
    else
	if (#2 date1 ) < (#2 date2) then
	    true
	else if ( #2 date1 ) > (#2 date2 ) then
	    false
	else
	    if (#3 date1)  < (#3 date2 ) then
		true
	    else
		false


fun number_in_month ( dates : (int * int * int) list , month : int ) =
    if null dates then
	0
    else
	let 
	    val tl_dates_number_in_month = number_in_month( tl dates , month)
	in
	    if (#2 (hd dates) )  = month  then
		tl_dates_number_in_month + 1
	    else
		tl_dates_number_in_month
	end

fun number_in_months ( dates: (int * int * int ) list , months : int list ) =
    if null months then
	0
    else
	number_in_month(dates,(hd months)) + number_in_months(dates,(tl months)) 


fun dates_in_month ( dates : (int * int * int) list , month : int )  =
    if null dates then
	[]
    else
	let
	    val tl_dates_in_month = dates_in_month((tl dates) ,month)
	in 
	    if (#2 (hd dates))  = month then 
		(hd dates) :: tl_dates_in_month
	    else
		tl_dates_in_month 		
	end


fun dates_in_months (dates : (int * int *int ) list , months : int list ) =
    if null months then
	[]
    else
	dates_in_month(dates,(hd months)) @ dates_in_months(dates,(tl months)) 
    

fun get_nth ( str  : string list , n : int )  =
    if n = 1 then
	hd str
    else
	get_nth ( (tl str)  , n -1 )


fun date_to_string ( date :  int * int * int  ) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September","October", "November", "December"]
    in
	get_nth( months , #2 date) ^ " " ^ Int.toString(#3 date ) ^ ", " ^ Int.toString ( #1 date ) 
    end


fun number_before_reaching_sum ( sum : int , elem : int list ) =
    if sum <= (hd elem)  then
	0
    else
	number_before_reaching_sum ( sum  - (hd elem ) , (tl elem ) ) + 1

fun what_month ( day_of_year  : int ) =
    let
	val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
	fun what_month_helper( day_of_year : int , days_in_month : int list ) =
	    if day_of_year <= (hd days_in_month) then
		1
	    else
		what_month_helper( day_of_year - (hd days_in_month) , (tl days_in_month) ) +1 
    in
	what_month_helper(day_of_year, days_in_month) 
    end

fun month_range ( day1 : int , day2 : int ) =
    if day1 > day2 then
	[]
    else
	what_month(day1) :: month_range(day1+1 , day2 )


fun oldest ( dates : ( int * int * int ) list  ) =
    if  null dates then
	NONE
    else
	let 
	    fun oldest_noneempty (dates : (int * int * int ) list ) =
		if null (tl dates ) then
		    hd dates 
		else
		    let
			val tl_dates_oldest = oldest_noneempty(tl dates)
		    in
			if is_older((hd dates) , tl_dates_oldest ) then
 			    hd dates
			else
			    tl_dates_oldest
		    end
	in
	    SOME(oldest_noneempty(dates))
	end


fun number_in_months_challenge (dates : ( int * int * int) list , months : int list ) =
    let
	fun is_in_list ( x : int ,  xs : int list ) =
	    if null xs then
		false 
	    else if x = ( hd xs ) then 
		true
	    else
		is_in_list ( x, (tl xs ) ) 
			   
	fun remove_duplicates ( months : int list ) =
	    if null months then
		[]
	    else 
		let
		    val rd_months = remove_duplicates( tl months )
		in
		    if is_in_list(hd months , rd_months) then
			rd_months
		    else
			(hd months) :: rd_months
		end
    in
	number_in_months(dates,remove_duplicates( months )) 
    end
	
fun dates_in_months_challenge (dates : ( int * int * int) list , months : int list ) =
    let
	fun is_in_list ( x : int ,  xs : int list ) =
	    if null xs then
		false 
	    else if x = ( hd xs ) then 
		true
	    else
		is_in_list ( x, (tl xs ) ) 
			   
	fun remove_duplicates ( months : int list ) =
	    if null months then
		[]
	    else 
		let
		    val rd_months = remove_duplicates( tl months )
		in
		    if is_in_list(hd months , rd_months) then
			rd_months
		    else
			(hd months) :: rd_months
		end
    in
	dates_in_months(dates,remove_duplicates( months )) 
    end
	
	

fun reasonable_date ( date : int * int * int )  =
    if (#1 date ) < 1 then
	false
    else
	if (#2 date) < 1 orelse (#2 date) >12  then
	    false
	else
	    let 
		val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
		fun get_nth ( xs : int list , n : int )  =
		    if n = 1 then
			hd xs
		    else
			get_nth(tl xs , n-1 ) 
	    in
		if (#3 date) < 1 then
		    false
		else
		    if (#2 date) = 2 then 
			if ((#1 date) mod 400) =0  orelse
			   ( ((#1 date) mod 4 ) = 0 andalso ((#1 date) mod 100) <> 0 ) then
			    if get_nth(days_in_month,(#2 date)) +1  < (#3 date) then
				false
			    else
				true
			else
			    if get_nth(days_in_month,(#2 date))   <  (#3 date) then
				false
			    else
				true
		    else
			if get_nth(days_in_month,(#2 date)) <  (#3 date) then
			    false
			else
			    true  
				
	    end

		


    

	

	

    

	    

	

							  
							  
							  
							  
