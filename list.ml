exception E;

fun empty([])= true
  | empty(x::xs) = false;

fun head([]) = raise E
  | head(x::xs) = x;

fun tail([]) = []
  | tail(x::xs) = xs;

fun attach(x,xs) = x::xs;

fun length([]) = 0
  | length(x::xs) = 1 + length(xs);

fun length(ls) =
	let
		fun iter(i,[]) = i
		  | iter(i,x::xs) = iter(i+1,xs)
	in
		iter(0,ls)
	end;

fun append([],l2) = l2
  | append(x::l1,l2) =  x::append(l1,l2)

fun rev([]) = []
  | rev(x::xs) =  append(rev(xs),[x]);

fun rev(ls) =
  let
	fun iter(revlist,[]) = revlist
          | iter(revlist,x::xs) = iter(x::revlist,xs)
  in
	iter([],ls)
  end;

fun insert(x,[]) = [x]
  | insert(x,y::ys) = 
	if (x <= y) then x::y::ys
        else   y::insert(x,ys)

fun insort([]) = []
  | insort(x::xs) =  insert(x,insort(xs));

fun msort([]) = []
  | msort([x]) = [x]
  | msort(ls) =
      let
	fun split(ls) =
		let
			fun iter([],i,l1,l2) = (l1,l2)
			  | iter(x::ls,i,l1,l2) =
				if (i mod 2 = 0) then iter(ls,i+1,x::l1,l2)
				else iter(ls,i+1,l1,x::l2)
 		in
			iter(ls,0,[],[])
		end
	fun merge([],l2) = l2
	  | merge(l1,[]) = l1
	  | merge(x::l1,y::l2) =
		if (x <= y) then x::merge(l1,y::l2)
		else y::merge(x::l1,l2)
	val (l1,l2) = split(ls)
      in
	merge(msort(l1),msort(l2))
      end;


fun filter pred [] = []
  | filter pred (x::ls) =
	if pred(x) then x::(filter pred ls)
	else (filter pred ls);

fun secr f x y = f(y,x);

fun qsort([]) = []
  | qsort([x]) = [x]
  | qsort(x::ls) =
       qsort(filter (secr op<= x) ls) @ x::qsort(filter (secr op> x) ls);


fun foldr f init [] = init
	| foldr f init (x::ls) = f(x, (foldr f init ls));

fun foldl f init [] = init
	| foldl f init (x::ls) = foldl f (f(x, init)) ls;

fun ok(x, y) = x-y;


fun depthfirst next pred ini =
	let
		fun dfs [] = []
		  | dfs(top::stack) =
			if pred(top) then top::dfs(stack)
			else dfs(next(top) @ stack)
	in
		dfs [ini]
	end;


fun isfull n qs = (n = length(qs));


fun upto(m,n) =
	if (m>n) then []
	else m::upto(m+1,n);

fun safequeen qs newq =
	let
		fun belongs(a,[]) = false
		  | belongs(a,x::ls) = (a = x) orelse belongs(a,ls)
		fun nodiag(newq,qs) =
			let
				fun iter(i,newq,[]) = true
				  | iter(i,newq,q::qs) =
					if (Int.abs(newq-q) = i ) then false
					else iter(i+1,newq,qs)
			in
				iter(1,newq,qs)
			end;

	in
		not(belongs(newq,qs)) andalso nodiag(newq,qs)
	end;
fun nextqueen n qs =  map (secr op:: qs) (filter (safequeen qs) (upto(1,n)))

fun queens n = (depthfirst (nextqueen n) (isfull n)) [];