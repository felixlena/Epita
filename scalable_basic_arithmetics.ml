(** Basic arithmetics for ordered euclidian ring. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB =
  let rec pgcd a b=
    if b=[0;0] then
      a
    else
      pgcd b (Scalable.mod_b a b)
  in
  pgcd (Scalable.mult_b (Scalable.from_int (Scalable.sign_b bA)) bA) (Scalable.mult_b (Scalable.from_int (Scalable.sign_b bB)) bB);;


(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  let rec loop (x, ui, vi)(y, uj, vj)=
    if y=[0;0] then
      
      (ui, vi, x)
    else
      let q=Scalable.quot_b x y  in
      let x2=Scalable.diff_b x (Scalable.mult_b q y) and u2=Scalable.diff_b ui (Scalable.mult_b q uj) and v2=Scalable.diff_b vi (Scalable.mult_b q vj) in
      loop (y, uj, vj)(x2, u2, v2)
  in
  loop (bA, [0;1], [0;0]) (bB, [0;0], [0;1]);;
