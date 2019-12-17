(* builtin.ml*)
let sign x=
  if x<0 then
    -1
  else
    1;;

let rec quot a b=
  if a-b>=0 then
    1+quot(a-b) b
  else
    0;;

let div a b=
  if b>0 then
    (quot a b, modulo a b)
  else
    invalid_arg "error : b==0";;


let rec  modulo x n=
  if x-n >=0 then
    modulo (x-n) n
  else
    x;;


(*FIN*)

(*basic_artihmetics.ml *)


let pgcd a b=
  let rec go a b=
    let r=a mod b in
    if r==0 then
      b
    else
      go b r
  in
  if b>a then
    go b a
  else
    go a b;;


let bezout a b=
  (* i=etape precendente j=etape precedent i *)
  let rec loop (x, ui, vi)(y, uj, vj)=
    if y==0 then
      (ui, vi)
    else
      let q=x/y  in
      let x2=x-q*y and u2=ui-q*uj and v2= vi-q*vj in
      loop (y, uj, vj)(x2, u2, v2)
  in
  loop (a, 1, 0) (b, 0, 1);;


(*FIN*)

(*power.ml*)

let pow x b=
  let rec loop z b=
    if b>0 then
      loop (z*x) (b-1)
    else
      z
  in
  loop 1 b;;

let power x b=
  let rec loop z b=
    if b==1 then
      z
    else if b mod 2==0 then
      loop (z*z) (b/2)
    else
      x*loop(x*x) ((b-1)/2)
  in
  if b==0 then
    1
  else
    loop x b;;

let mod_power b e n=
  let rec loop i c=
    let c=(b*c) mod n in
    if i==e then
      c
    else
      loop (i+1) c
  in
  loop 1 1;;

let prime_mod_power a b n=
  let r= b mod (n-1) in
  mod_power a r n;;


(*FIN*)

(*test_primes*)

let is_prime n=
  let rec loop i=
    if i==n then
      true
    else if n mod i ==0 then
      false
    else
      loop (i+1)
  in
  if n<1 then
    invalid_arg "error : n<1"
   else if n==2 then
      true
   else
     loop 3;;
      
(* Generate prime *)

let init_eratosthenes n=
  let rec loop i=
    if i>n then
      []
    else
      i::loop (i+1)
  in
  2::loop 3;;

let eratosthenes n=
  let rec delete l k=
    match l with
	[]->[]
      |e::l ->
	if e mod k =0 && e <> k then
	  delete l k
	else
	  e::delete l k
  in
  let rec loop l i=
    if i=n then
      l
    else
      loop (delete l i) (i+1)
  in
  loop (init_eratosthenes n) 2;;


let double_primes n fct=
  let rec loop i=
    if n=i then
      []
    else if fct i && fct (2*i+1) then
      (i, 2*i+1)::loop (i+1)
    else
      loop (i+1)
  in
  loop 2;;
  
(*FIN*)

(* cipher *)
let rec encrypt_cesar key word n=
  match word with
      [] -> []
    |e::l -> let c= (e+key) mod n in
	     if n<0 then
	       let c=c+n in c::encrypt_cesar key l n
	     else
	       c::encrypt_cesar key l n;;

let decrypt_cesar key word n= encrypt_cesar (-key) word n;;


let is_inverse x y n = modulo ((modulo x n) * (modulo y n)) n = 1
  
let generate_keys_rsa p q=
  let n= p*q in
  let phi= (p-1)*(q-1) in
  let rec find_exposant_de_chiffrement i=
    if pgcd phi i = 1 then
      i
    else
      find_exposant_de_chiffrement (i+1)
  in
  let e = find_exposant_de_chiffrement 2
  in let rec find_d i=
       if (i*e) mod phi=1 then
	 i
       else
	 find_d (i+1)
  in
  let d= find_d 1
  in ((n, e),(n,d));;

let encrypt_rsa m (n,e)= mod_power m e n;;

let decrypt_rsa c (n,d)= mod_power c d n;;

(*FIN*)

(* break_ciphers.ml *)

let break (a,b)=0;;
    
  
(*FIN*)


(* SCALABLE.ML *)

let from_int x=
  let rec intToBin x d l=
    if d=0 then
      l
    else
      intToBin (x mod d)(d/2)((x/d)::l)
  and c2 l change=
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
    intToBin x 128 [0]
  else
    c2(intToBin (-x) 128 [0]) false;;



(*888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888*)


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
  let rec loop l1 l2=
    match l1 with
      []->l2
      | e::l1-> loop l1 (e::l2)
  in
  match bA with
      []|_::[] -> []
    |e::l-> e:: loop l [];;

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
  let rec length l=
    match l with
        []->0
      |e::l -> 1+length l
  in
  let rec loop l1 l2 grand petit=
    match (l1,l2) with
      |(_,[])|([],_)-> 0
      |(e1::l1, e2::l2) ->
        if e1= grand && e2=petit then 1
        else if e1= petit && e2=grand then -1
        else
          loop l1 l2 grand petit
  in
  match (l1,l2) with
    |(_,[])|([],_)-> -10 (*impossible*)
    |(e1::l1,e2::l2) ->
      if e1 = 1 && e2 = 0 then -1
      else if e1=0 && e2=1 then 1
      else
	let sign= if e1=0 then 1 else -1 in
	let len1=length l1 and len2=length l2 in
	if len1>len2 then 1*sign
	else if len2>len1 then -1*sign
	else
	  if sign = 1 then
	    loop l1 l2 1 0
	  else
	    loop l1 l2 0 1;;

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
      []|_::[]->-2 (*impossible*)
      |e::_ -> if e=0 then 1 else -1

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
let add_n nA nB = []

(* Difference of two naturals.
    UNSAFE: First entry is assued to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB = []

(* Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = []

(* Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = []

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
