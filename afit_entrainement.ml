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
    if i== n then
      true
    else if n mod i ==0 then
      false
    else
      loop (i+1)
  in
  if n<1 then
    invalid_arg "error : n<1"
  if n==2 then
    true
