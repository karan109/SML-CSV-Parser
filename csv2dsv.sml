(*Author - Karan Aggarwal
Entry Number - 2019CS10699
Date - 24/02/2021*)

exception emptyInputFile; (* Empty input file exception *)
exception UnevenFields of string; (* Uneven fields in CSV *)
exception noNewLine; (* No new line at end of file *)


fun convertDelimiters(infilename, delim1, outfilename, delim2) =
	let
		val fi = TextIO.openIn(infilename) (* Open input file *)
		val si = TextIO.inputAll(fi) (* Read input file contents into a string *)
		val close = TextIO.closeIn(fi) (* Close input file *)

		(* Length of a list *)
		fun length([]) = 0
  			| length(x::ls) = 1+length(ls)

  		(* Append ls1 to ls2 *)
		fun append([], ls2) = ls2
  			| append(x::ls1, ls2) =  x::append(ls1, ls2)
		
  		(* Reverse list *)
		fun rev([]) = []
  			| rev(x::ls) =  append(rev(ls), [x]);

  		(* Head of a list *)
  		fun head([]) = raise emptyInputFile
			| head(x::ls) = x

  		(* Split list on the basis of token *)
		fun split(parity, temp, token, []) = [temp]
			| split(parity, temp, token, x::ls) = 
				if x = token andalso parity = 0 then
					temp::split(parity, [], token, ls)
				else if x = token then
					split(parity, x::temp, token, ls)
				else if x = #"\"" then
					split(1-parity, x::temp, token, ls)
				else
					split(parity, x::temp, token, ls)

		(* Check if char list starts with " *)
		fun checkquote([]) = false
			| checkquote(x::ls) = 
				if x = #"\"" then true
				else false

		(* Remove beginning and ending " in the list, if they exist *)
		fun removequote([]) = []
			| removequote(x::ls) = 
				let fun temp([]) = [] | temp(y::lt) = lt
				in
					if checkquote(x::ls) = true then rev(temp(rev(ls)))
					else x::ls
				end

		(* Add " to beginning and end of the list if they do not exist *)
		fun addquote(ls) = 
			if checkquote(ls) = false then #"\""::(ls@[#"\""])
			else ls

		(* Check if list contains ", \n, or delim2 *)
		fun contains([]) = 0
			| contains(x::ls) = 
				if x = #"\"" orelse x = delim2 orelse x = #"\n" then 1
				else contains(ls)

		(* Add quote to list if it contains ", \n, or delim2 *)
		fun process(ls) =
			if contains(removequote(ls)) = 1 then addquote(ls)
			else ls

		(* Split char list on the basis of delim2 *)
		fun splitdelim([]) = []
			| splitdelim(x::ls) = 
				let val temp = split(0, [], delim1, x)
					val temp2 = map process temp
				in
					rev(temp2)::splitdelim(ls)
				end

		(* Join a char list with token *)
		fun join token [] = []
			| join token [x] = x
			| join token (x::ls) = append(x, token::(join token ls))

		(* Checks for Uneven fields exception *)
		fun check([], n, ct) = (~1, ~1)
			| check(x::ls, n, ct) = 
				if x <> n then (x, ct)
				else check(ls, n, ct+1)
	in
		if si = ""  then
			raise emptyInputFile (* Empty input file *)
		else 
			let
				val s = String.substring(si, 0, String.size(si)-1) (* Ignore the last newline character *)
				
				(* x is a list of char lists of different lines *)
				val x = split(0, [], #"\n", String.explode(s))

				(* z is a list of list of char lists of different fields of csv *)
				val z = splitdelim(x)

				(* lengths gives a list of number of fields in each line *)
				val lengths = map length z

				(* y forms a string with the desired output *)
				val y = String.implode(join #"\n" (map (join delim2) z))

				val fields = head(lengths) (* Desired number of fields (Fields in line 1) *)

				val (occur, line) = check(lengths, fields, 1) (* Gets the details of uneven fields, if any *)
			in
				(* No new line at end of file *)
				if String.sub(si, String.size(si)-1) <> #"\n" then raise noNewLine

				(* Exception raised of unevenFields *)
				else if occur <> ~1 then raise UnevenFields("Expected: "^Int.toString(fields)^" fields, Present: "^Int.toString(occur)^" fields on Line "^Int.toString(line)^"\n")
				
				(* Output result in the file after adding a newline according to convention *)
				else 
					let 
						val fo = TextIO.openOut(outfilename) (* Open output file *)
					in
						TextIO.output(fo, String.concat([y, "\n"]))
						before TextIO.closeOut(fo) (* Close output file *)
					end
			end
		handle UnevenFields(ex) => TextIO.output(TextIO.stdOut, ex) (* Handle exception *)
	end;


fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t");


fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t", outfilename, #",");