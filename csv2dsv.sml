Control.Print.printLength := 100;
exception emptyInputFile; (* Empty input file exception *)
exception UnevenFields of string; (* Uneven fields in CSV *)
fun convertDelimiters(infilename, delim1, outfilename, delim2) =
	let
		val fi = TextIO.openIn(infilename) (* Open input file *)
		val fo = TextIO.openOut(outfilename) (* Open output file *)
		val si = TextIO.inputAll(fi) (* Read input file contents into a string *)
		val s = String.substring(si, 0, String.size(si)-1) (* Ignore the last newline character *)
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

		(* x is a list of char lists of different lines *)
		val x = split(0, [], #"\n", String.explode(s))
		(* z is a list of list of char lists of different fields of csv *)
		val z = splitdelim(x)
		(* lengths gives a list of number of fields in each line *)
		val lengths = map length z
		(* y forms a string with the desired output *)
		val y = String.implode(join #"\n" (map (join delim2) z))

		(* Checks for Uneven fields exception *)
		fun check([], n, ct) = (~1, ~1)
			| check(x::ls, n, ct) = 
				if x <> n then (x, ct)
				else check(ls, n, ct+1)
	in
		if s = "" then
			raise emptyInputFile (* Empty input file *)
		else 
			let
				val fields = head(lengths) (* Desired number of fields (Fields in line 1) *)
				val (occur, line) = check(lengths, fields, 1) (* Gets the details of uneven fields, if any *)
			in
				(* Exception raised of unevenFields *)
				if occur <> ~1 then raise UnevenFields("Expected: "^Int.toString(fields)^" fields, Present: "^Int.toString(occur)^" fields on Line "^Int.toString(line)^"\n")
				
				(* Output result in the file after adding a newline according to convention *)
				else TextIO.output(fo, String.concat([y, "\n"]))
			end

		handle UnevenFields(ex) => TextIO.output(TextIO.stdOut, ex) (* Handle exception *)
		before TextIO.closeOut(fo) (* Close output file *)
	end;


fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename, #",", outfilename, #"\t");


fun tsv2csv(infilename, outfilename) = convertDelimiters(infilename, #"\t", outfilename, #",");


fun convertNewlines(infilename, newline1, outfilename, newline2) = 
	let
		val fi = TextIO.openIn(infilename) (* Open input file *)
		val fo = TextIO.openOut(outfilename) (* Open output file *)
		val s = TextIO.inputAll(fi) (* Read input file contents into a string *)
		val close = TextIO.closeIn(fi) (* Close input file *)
		val len1 = String.size(newline1) (* Length of newline1 string *)
		val len2 = String.size(newline2) (* Length of newline2 string *)
		val l1 = String.explode(newline1) (* newline1 as list of chars *)
		val l2 = String.explode(newline2) (* newline2 as list of chars *)

		(* Concatenate 2 lists *)
		fun append([], ls2) = ls2
  			| append(x::ls1, ls2) =  x::append(ls1, ls2)

  		(* Compare if 2 lists are equal in element-wise comparison *)
  		fun compare([], []) = true
  			| compare([], x::ls) = false
  			| compare(x::ls, []) = false
  			| compare((x:char)::ls1, (y:char)::ls2) = 
  				if x = y then compare(ls1, ls2)
  				else false

  		(* Get the first n entries of a list *)
  		fun getN([], n) = []
  			| getN(x::ls, n) = 
  				if n = 0 then []
  				else x::getN(ls, n-1)

  		(* Get the remaining entries of the list (after discarding the first n) *)
  		fun getRest([], n) = []
  			| getRest(x::ls, n) = 
  				if n = 0 then x::ls
  				else getRest(ls, n-1)

  		(* New line change main function *)
  		fun process([]) = []
			| process(x::ls) = 
				if compare(getN(x::ls, len1), l1)  = true then
					append(l2, process(getRest(x::ls, len1))) (* Replace newline1 by newline2 *)
				else x::process(ls)
	in
		if s = "" then
			raise emptyInputFile (* Empty input file *)
		else
			(* Send a char array for processing and print output to outputfile *)
			TextIO.output(fo, String.implode(process(String.explode(s))))
		before TextIO.closeOut(fo) (* Close output file *)
	end;


fun unix2dos(infilename, outfilename) = convertNewlines(infilename, "\n", outfilename, "\r\n");


fun dos2unix(infilename, outfilename) = convertNewlines(infilename, "\r\n", outfilename, "\n");


(*convertDelimiters("TestCases/himym.csv", #",", "TestCases/out.txt", #"|");*)
(*convertNewlines("ok.txt","\n","out.txt","\r\n");*)
(*unix2dos("ok.txt", "out.txt");*)