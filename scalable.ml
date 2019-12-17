(* A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

(* Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let reverse_n bA=
  let rec loop l1 l2=
    match l1 with
      []->l2
      | e::l1-> loop l1 (e::l2)
  in
  loop bA [];;


let reverse_b bA=
  match bA with
      []->[]
    |e::l->e::reverse_n l;;

let from_int x =
  let rec find_start x i=
    if i >= x then
      i
    else
      find_start x (i*2)
        in
  let rec intToBin x d l=
    if d=0 then
      l
    else
      intToBin (x mod d)(d/2)((x/d)::l)
  and  c2 l change=
    match l with
       []->[]
      |e::l->
	if change then
	  if e=0 then
	    1::c2 l true
	  else
	    0::c2 l true
	else
	  if e=1 then
	    e::c2 l true
	  else
	    e::c2 l false
  in
  if x>=0 then
    let d=find_start x 1 in
    0::intToBin x d []
  else
      let d=find_start (-x) 1 in
    1::c2(intToBin (-x) d []) false;;

(* Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)


let to_int n=
  let rec pow l i=
   match l with
        []->i
      |_::l -> pow l (i*2)
  in
  let rec sum l m z=
    match l with
      |[] -> 0
      |e1::[]-> (z+e1*m)
      |e::l -> sum l (m*2) (z+e*m)
  in
   match n with
      []->0
     |e::l ->
       if e=0 then
         sum l 1 0
       else
         -pow l 1 + sum l 1 0;;

(* Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let  print_b n=
  let n=reverse_b n in
  let rec loop l=
    match l with
      |[]->()
      |e::l->print_int(e);
        loop l
  in
   loop n;;
(* Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(* Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(* Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let compare_n n1 n2=
  let n1=reverse_n (n1) and n2=reverse_n (n2) in
  let rec length l=
    match l with
        []->0
      |e::l -> 1+length l
  in
  let rec loop l1 l2=
    match (l1,l2) with
      |([],_)|(_,[])-> 0
      |(e1::l1, e2::l2) ->
        if e1>e2 then 1
        else if e1<e2 then
          -1
        else
          loop l1 l2
  in
  let len1=length n1 and len2=length n2 in
  if len1>len2 then 1
  else if len2>len1 then -1
  else
  loop n1 n2;;
  (*let len1=String`.length n1 and len2=String.length n2 in
                if len1>len2 then
                  1
                else if len1<len2 then
                  -1
                else
                  loop n1 n2;; *)
(* Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)


let (>>!) a b= if compare_n  a b = 1 then true else false;;

(* Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) a b= if compare_n  a b = -1 then true else false;;

(* Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) a b= let c = compare_n a b in if c = 1 || c = 0   then true else false;;
(* Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) a b= let c = compare_n a b in if c = -1 || c = 0   then true else false;;
(* Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b l1 l2=
  let l1=reverse_b (l1) and l2=reverse_b (l2) in
  match (l1,l2) with
    |([],_)|(_,[]) -> 0
    |(e1::l1,e2::l2)->
      (match (e1,e2) with
        |(0,0)->compare_n l1 l2
        |(1,1)-> -1*compare_n l1 l2
        |(0,1)->1
        |(_,_)->(-1));;

(* Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) a b= if compare_b  a b = -1 then true else false;;

(* Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) a b= if compare_b  a b = 1 then true else false;;

(* Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) a b= let c = compare_b a b in if c = -1 || c = 0   then true else false;;

(* Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) a b= let c = compare_b a b in if c = 1 || c = 0   then true else false;;

(* Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  match bA with
      []-> 0
    |e::_-> if e=1 then 1 else -1;;

(* Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  if sign_b bA =1 then bA
  else
    let rec c2 l change=
    match l with
       []->[]
      |e::l->
	if change then
	  if e=0 then
	    1::c2 l true
	  else
	    0::c2 l true
	else
	  if e=1 then
	    e::c2 l true
	  else
	    e::c2 l false
    in
    match bA with
        []|_::[]-> [] (*impossible*)
      |e::l ->0:: c2 l false;;

(* Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = 0

(* Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = 0

(* Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (0, 0)

(* Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  let rec loop l1 l2 retenue=
    match (l1,l2) with
      |([],[]) when retenue->[1]
      |([],[])->[]
      |([],e2::l2)->e2::loop l1 l2 false
      |(e1::l1,[])->e1::loop l1 l2 false
      |(e1::l1, e2::l2)->
	(match (e1,e2) with
	  |(0,0) when retenue -> 1::loop l1 l2 false
	  |(0,0)-> 0::loop l1 l2 false
	  |(1,1) when retenue -> 1::loop l1 l2 true
	  |(1,1)-> 0::loop l1 l2 true
	  |(_,_) when retenue -> 0::loop l1 l2 true
	  |(_,_) -> 1::loop l1 l2 false)
  in loop nA nB false;;


(* Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let rec loop l1 l2 retenue=
    match (l1,l2) with
      |([],[]) when retenue->[1]
      |([],[])->[]
      |([],e2::l2)->e2::loop l1 l2 false
      |(e1::l1,[])->e1::loop l1 l2 false
      |(e1::l1, e2::l2)->
	(match (e1,e2) with
	  |(0,0) when retenue -> 1::loop l1 l2 true
	  |(0,0)-> 0::loop l1 l2 false
	  |(1,1) when retenue -> 1::loop l1 l2 true
	  |(1,1)-> 0::loop l1 l2 false
	  |(1,0) when retenue -> 0::loop l1 l2 false
          |(1,0) -> 1::loop l1 l2 false
          |(0,1) when retenue -> 0::loop l1 l2 true
          |(_,_) -> 1::loop l1 l2 true)
  in
  let simplifier l=
    let rec loop l=
      match l with
          []->[0;0]
        |e1::l1 -> if e1=0 then loop l1 else l
    in
    reverse_n (loop(reverse_n l))
  in
 simplifier(loop nA nB false);;

(* Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = []

(* Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =
  match (bA,bB) with
      ([],_)|(_,[])->[]
    |(e1::l1,e2::l2)->(diff_n l1 l2);;

(* Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = []

(* Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = []

(* Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB = []

(* Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = []

(* Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ([], [])
