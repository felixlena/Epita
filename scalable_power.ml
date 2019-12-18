(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class in the
   built-in integer case.
*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  let rec loop z b=
    if (>>) b [0;0] then
      loop (Scalable.mult_b z x) (Scalable.diff_b b [0;1])
    else
      z
  in
  match x with
    |[0;0] -> [0;0]
    |[0;1]-> [0;1]
    |[1;1]->
      if Scalable.mod_b n [0;0;1] = [0;0] then
        [0;1]
      else
        [1;1]
    |_ ->loop [0;1] n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  let rec loop z b=
    if b=[0;0] then
      [0;1]
    else if Scalable.mod_b b [0;0;1]=[0;0] then
      loop (Scalable.mult_b z z) (Scalable.quot_b b [0;0;1])
    else
      Scalable.mult_b z ( loop (Scalable.mult_b z z) (Scalable.quot_b b [0;0;1]))
  in
  match x with
    |[0;0] -> [0;0]
    |[0;1]-> [0;1]
    |_ ->loop x (n);;

(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power b e n=
  let rec loop i c=
    let c= Scalable.mod_b (Scalable.mult_b b c) n in
    if i=Scalable.diff_b e [0;1] then
      c
    else
      loop (Scalable.add_b i [0;1]) c
  in
  match b with
    |[0;0] -> [0;0]
    |[0;1]-> [0;1]
    |_ when e=[0;0] -> [0;1]
    |_ when e=[0;1] -> Scalable.mod_b b n
    |_ -> Scalable.mod_b (loop [0;1] b) n;;


(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p = []
