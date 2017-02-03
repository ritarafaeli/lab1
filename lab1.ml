
let exercise2 () = 42 ;;

let exercise3 () = (-) 5 3 ;;

let exercise5a : int = 42 ;;

let exercise5b : string =
  let greet y = "Hello " ^ y
  in greet "World!";;

let exercise5c : int * float -> int =
  fun (x, y) -> x + int_of_float y ;;

let exercise5d : int -> bool =
  fun x -> x < x + 1 ;;

let exercise5e : bool -> bool list =
  fun x -> if x then [x] else [] ;;

let exercise5f : int option list =
  [Some 4; Some 2; None; Some 3] ;;

let exercise5g : ('a option * float option) * bool =
  ((None, Some 42.0), true) ;;

let rec square_all (lst : int list) : int list =
  match lst with
	| [] -> []
	| t::h -> (t * t):: square_all h;;

let exercise6 = assert(square_all([2;4])=[4;16]) ;;

let rec sum (lst : int list) : int =
  match lst with
  | [] -> 0
  | h::t -> h + sum t ;; 
  
let rec max_list (lst : int list) : int =
	match lst with
	| [] -> raise (Invalid_argument "empty list")
	| h::[] -> h
	| h::t -> max h (max_list t);;

let rec zip (x : int list) (y : int list) : (int * int) list =
  match x,y with
	| [],[] -> []
	| l,[]-> raise (Invalid_argument "mistmatched list lengths")
	| [],l-> raise (Invalid_argument "mistmatched list lengths")
	| h1::[],h2::[] -> (h1,h2)::[]
	| h1::t1,h2::t2 -> (h1,h2) :: zip t1 t2 ;;

let rec prods (lst : (int * int) list) : int list =
  match lst with
  | [] -> []
  | (x, y) :: tail -> (x * y) :: (prods tail) ;;

let dotprod (a : int list) (b : int list) : int =
	sum (prods (zip a b));;
	

open List;;


let sum_ho (lst : int list) : int =
	match lst with 
	| [] -> 0
	| h::t -> fold_left (+) h t ;;


let mult (x,y) = x * y;;

let prods_ho (lst : (int * int) list) : int list =
  List.map mult lst ;;

let pluck (x : int) (y : int) : (int * int) =
  (x,y);;
		
let zip_ho (x : int list) (y : int list) : (int * int) list =
  List.map2 pluck x y;;

let is_even (x : int) : bool =
	if x mod 2 == 0 then true else false;;

let evens (l : int list) : int list =
  List.filter is_even l;;
