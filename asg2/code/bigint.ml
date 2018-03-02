(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(*
PARTNER 1: VIEN VAN vhvan
PARTNER 2: AKOBIR KHAMIDOV akhamido *)

open Printf

module Bigint = struct

type sign     = Pos | Neg
type bigint   = Bigint of sign * int list
let  radix    = 10
let  radixlen =  1

let car       = List.hd
let cdr       = List.tl
let map       = List.map
let reverse   = List.rev
let strcat    = String.concat
let strlen    = String.length
let strsub    = String.sub
let zero      = Bigint (Pos, [])

let charlist_of_string str =
	let last = strlen str - 1
	in  let rec charlist pos result =
	if pos < 0 then result
	else charlist (pos - 1) (str.[pos] :: result)
	in charlist last []

let bigint_of_string str =
	let len = strlen str
	in  let to_intlist first =
	let substr = strsub str first (len - first) in
	let digit char = int_of_char char - int_of_char '0' in
	map digit (reverse (charlist_of_string substr))
	in if len = 0 then zero
	else if str.[0] = '_'
		then Bigint (Neg, to_intlist 1)
		else Bigint (Pos, to_intlist 0)


let string_of_bigint (Bigint (sign, value)) =
	match value with
	| []    -> "0"
	| value -> let reversed = reverse value
				in  strcat ""
				((if sign = Pos then "" else "-") ::
				(map string_of_int reversed))
(*
 Helper Functions
 *)
	(* 1 if a > b
	   0 if a = b
	  -1 if a < b *)
let rec cmp list1 list2 sign = match (list1, list2) with
	| [], [] -> sign
	| list1, [] -> 1
	| [], list2 -> -1
	| car1 :: cdr1, car2 :: cdr2 ->
		cmp cdr1 cdr2
		(if car1 = car2 then sign else
		if car1 > car2 then 1 else -1)

let rec add' list1 list2 carry = match (list1, list2, carry) with
	| list1, [], 0       -> list1
	| [], list2, 0       -> list2
	| list1, [], carry   -> add' list1 [carry] 0
	| [], list2, carry   -> add' [carry] list2 0
	| car1::cdr1, car2::cdr2, carry ->
		let sum = car1 + car2 + carry
		in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

let rec sub_helper list1 list2 carry = match (list1, list2, carry) with
	| list1, [], 0      -> list1
	| [], list2, 0      -> list2
	| list1, [], carry  -> sub_helper list1 [carry] 0
	| [], list2, carry  -> sub_helper [carry] list2 0
	| car1::cdr1, car2::cdr2, carry ->
		let diff = if (car1 - carry) >= car2 then (car1 - car2 - carry)
		else ((10 + car1 - carry) - car2)
		in diff :: sub_helper cdr1 cdr2
			(if (car1 - carry) >= car2 then 0 else 1)

let trimzeros list =
	let rec trimzeros' list' = match list' with
	| []       -> []
	| [0]      -> []
	| car::cdr ->
		let cdr' = trimzeros' cdr
		in  match car, cdr' with
			| 0, [] -> []
			| car, cdr' -> car::cdr'
				in trimzeros' list

let sub' list1 list2 carry = trimzeros (sub_helper list1 list2 carry)

let double num_list = (add' num_list num_list 0)

let rec mul' (multiplier, powerof2, multiplicand') =
	let comp = cmp powerof2 multiplier 0 in
	if comp = 1 then multiplier, [0]
	else let remainder, product =
	   mul' (multiplier, double powerof2, double multiplicand')
	   in  let comp2 = cmp powerof2 remainder 0 in
	   if (comp2 = 1) then remainder, product
	   else (sub' remainder powerof2 0), (add' product multiplicand' 0)

let mul_helper (multiplier, multiplicand) =
	let _, product = mul' (multiplier, [1], multiplicand)
	in product

let rec divrem' (dividend, powerof2, divisor') =
	if (cmp divisor' dividend 0 = 1)
	then [0], dividend
	else let quotient, remainder =
		divrem' (dividend, double powerof2, double divisor')
		in  if (cmp divisor' remainder 0 = 1)
		then quotient, remainder
		else (add' quotient powerof2 0), (sub' remainder divisor' 0)

let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

let div_helper (dividend, divisor) =
	let quotient, _ = divrem (dividend, divisor)
	in quotient

let rem_helper (dividend, divisor) =
	let _, remainder = divrem (dividend, divisor)
	in remainder

let even number =
	(if [0] = (rem_helper (number, [2]))
	then true else false)

let rec power' (base, expt, result) = match expt with
	| []                  -> result
	| expt when even expt ->
	power' ((mul_helper (base, base)), (div_helper (expt,  [2])), result)
	| expt                ->
	power' (base, (sub' expt [1] 0), (mul_helper (base, result)))


let power_helper (base, expt) =
	power' (base, expt, [1])




(* Functions *)

let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if (neg1 = neg2)
	then Bigint ((if neg1 = Neg then Neg else Pos), add' value1 value2 0)
	else let comp = cmp value1 value2 0 in
	if (comp = 1)
	then Bigint (neg1, sub' value1 value2 0)
	else Bigint (neg2, sub' value2 value1 0)

let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if (neg1 = neg2)
	then let comp = cmp value1 value2 0 in
	if (comp = 1)
	then Bigint (neg1, sub' value1 value2 0)
	else Bigint (
		(if neg2 = Pos then Neg else Pos), sub' value2 value1 0)
			else Bigint (
				(if neg1 = Neg then Neg else Pos), add' value1 value2 0)

let mul (Bigint (neg1, value1)) (Bigint (neg2, value2))=
	Bigint (
	(if neg1 = neg2 then Pos else Neg),
	mul_helper (value1, value2))

let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	Bigint (Pos, div_helper (value1, value2))

let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	Bigint (Pos, rem_helper (value1, value2))

let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if(neg2 = Neg) then zero
	else Bigint (Pos, power_helper (value1, value2))

end
