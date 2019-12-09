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
    intToBin bA 128 [0]
  else
    c2(intToBin (-bA) 128 [0]) false;;



let to_int n=
  let rec sum l m z=
    match l with
      |[]|_::[] -> 0	
      |e1::e2::[]-> if e2=0 then (z+e1*m) else -(256-(z+e1*m))
      |e::l -> sum l (m*2) (z+e*m)
  in
   match n with
      []->0
     |l -> sum l 1 0;;

let rec print_b n=
  match n with
      []->()
    |e::l->print_int(e);
      print_string(" ");
      print_b l;;

(* compare naturals*)

let compare_n n1 n2=
  if n1>n2 then
    1
  else if n1=n2 then
    0
  else
    -1;;

let (>>!) a b= if compare_n  a b = 1 then true else false;;
let (<<!) a b= if compare_n  a b = -1 then true else false;;
let (>=!) a b= let c = compare_n a b in if c = 1 || c = 0   then true else false;;
let (<=!) a b= let c = compare_n a b in if c = -1 || c = 0   then true else false;;

(*compare bits*)

let compare_b l1 l2=
  let n1=to_int l1 and n2=to_int l2 in
  if n1>n2 then
    1
  else if n1=n2 then
    0
  else
    -1;;

let (>>) a b= if compare_b  a b = 1 then true else false;;
let (<<) a b= if compare_b  a b = -1 then true else false;;
let (>>=) a b= let c = compare_b a b in if c = 1 || c = 0   then true else false;;
let (<<=) a b= let c = compare_b a b in if c = -1 || c = 0   then true else false;;


let a=from_int (-10);;
let b=from_int 10;;
compare_b a b;;
