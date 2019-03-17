(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"
It has been adopted from the blog article:
http://gallium.inria.fr/blog/kmp/
Copyright (c) François Pottier
*)

(****************************************************)
(*               Flag for output printing           *)
(****************************************************)

let out = ref false

(****************************************************)
(* Reshaping the naive algorithm with a single loop *)
(****************************************************)

open String

let naive_search_one_loop text pattern = 
  let n = length text in
  let m = length pattern in
  if n < m then None
  else
    let k = ref 0 in
    let j = ref 0 in
    let stop = ref false in
    let res = ref None in
    while !k <= n && not !stop do
      if !j = m
      then (
        res := Some (!k - !j);
        stop := true)
      else if !k = n then (stop := true)
      else if text.[!k] = pattern.[!j]
      then (
        k := !k + 1;
        j := !j + 1)
      else  (
        k := !k - !j + 1;
        j := 0)
    done;
    !res



(****************************************************)
(*                Making it recursive               *)
(****************************************************)

type search_result = 
  | Found of int
  | Interrupted of int



let global_search search text pattern = 
  let n = length text in
  let m = length pattern in
  let res = search pattern m text n 0 0 in
  match res with 
  | Found x -> Some x
  | _ -> None

let search_rec = 
  let rec search pattern m text n j k =
    if j = m then
      Found (k - j)
    else if k = n then
      Interrupted j
    else if pattern.[j] = text.[k] then
      search pattern m text n (j + 1) (k + 1)
    else
      search pattern m text n 0 (k - j + 1)
  in
  global_search search

(*****************************************************)
(*             Instrument with invariant             *)
(*****************************************************)


let search_inv = 
  let rec search pattern m text n j k =
    assert (0 <= j && j <= m);
    assert (j <= k && k <= n);
    assert (sub pattern 0 j = sub text (k - j) j);
    
    if j = m then
      Found (k - j)
    else if k = n then
      Interrupted j
    else if pattern.[j] = text.[k] then
      search pattern m text n (j + 1) (k + 1)
    else
      search pattern m text n 0 (k - j + 1)
  in
  global_search search

(**
Search within a long text can be split up as a sequential composition of two searches.
Here's an equivalence for k <= l <= n
search pattern m text n j k
is equivalent to 
 let result = search pattern m text l j k in
  match result with
  | Found _ ->
      result
  | Interrupted j' ->
      search pattern m text n j' l
As the interruption happened when we hit the imposed right end of the text (i.e., l).
**)

(*****************************************************)
(*    Fast-Forwarding Search using Interrupt Index   *)
(*****************************************************)


open Printf

let search_with_shift = 
  let rec search pattern m text n j k =
  if j = m then
    Found (k - j)
  else if k = n then
    Interrupted j
  else if pattern.[j] = text.[k] then
    search pattern m text n (j + 1) (k + 1)
  else if j = 0 then
    search pattern m text n 0 (k + 1)
  else 
    let result = search pattern m text k 0 (k - j + 1) in
    match result with
    | Found _ ->
        result
    | Interrupted j' -> search pattern m text n j' k
  in
  global_search search


let search_with_shift_print = 
  let rec search pattern m text n j k =
  if j = m then
    Found (k - j)
  else if k = n then
    Interrupted j
  else if pattern.[j] = text.[k] then
    search pattern m text n (j + 1) (k + 1)
  else if j = 0 then
    search pattern m text n 0 (k + 1)
  else
    (* Determine for how much to fast-forward j to start with k *)
    (if !out then (
        printf "Failed at j = %d, k = %d\n" j k; 
        printf "Now figuring out by how much to shift pattern \"%s\" agains prefix \"%s\"\n"
          pattern (sub text (k - j + 1) (j - 1)));
    let result = search pattern m text k 0 (k - j + 1) in
    match result with
    | Found _ ->
        result
    | Interrupted j' -> (
        if !out 
        then printf "Starting the subsequent match from j'= %d\n\n" j';
        search pattern m text n j' k)
    )

  in
  global_search search


(*****************************************************)
(*        Extracting the Interrupt Index             *)
(*****************************************************)


let assertInterrupted = function
  | Found _       -> assert false
  | Interrupted j -> j


let search_assert = 
  let rec search pattern m text n j k =
  if j = m then
    Found (k - j)
  else if k = n then
    Interrupted j
  else if pattern.[j] = text.[k] then
    search pattern m text n (j + 1) (k + 1)
  else if j = 0 then
    search pattern m text n 0 (k + 1)
  else
    let j' = assertInterrupted @@ search pattern m text k 0 (k - j + 1) in
    search pattern m text n j' k
  in
  global_search search


(*****************************************************)
(*        Exploiting the Prefix Equality             *)
(*****************************************************)


let search_via_pattern =
  let rec search pattern m text n j k =
  if j = m then
    Found (k - j)
  else if k = n then
    Interrupted j
  else if pattern.[j] = text.[k] then
    search pattern m text n (j + 1) (k + 1)
  else if j = 0 then
    search pattern m text n 0 (k + 1)
  else
    (* So we're looking in our own prefix *)
    let j' = assertInterrupted @@ search pattern m pattern j 0 1 in
    assert (j' < j);
    search pattern m text n j' k

  in 
  global_search search

(*****************************************************)
(*        Tabulating the interrupt indices           *)
(*****************************************************)


let rec loop table pattern m text n j k =
  if j = m then
    Found (k - j)
  else if k = n then
    Interrupted j
  else if pattern.[j] = text.[k] then
    loop table pattern m text n (j + 1) (k + 1)
  else if j = 0 then
    loop table pattern m text n 0 (k + 1)
  else
    loop table pattern m text n table.(j) k


(****************************************************)
(*              Initialising the table              *)
(****************************************************)

let search_with_inefficient_init =

  let loop_search pattern _ text n j k = 
    let rec search pattern m text n j k =
      if j = m then
        Found (k - j)
      else if k = n then
        Interrupted j
      else if pattern.[j] = text.[k] then
        search pattern m text n (j + 1) (k + 1)
      else if j = 0 then
        search pattern m text n 0 (k + 1)
      else
        (* So we're looking in our own prefix *)
        let j' = assertInterrupted @@ search pattern m pattern j 0 1 in
        assert (j' < j);
        search pattern m text n j' k
    in
    
    let m = length pattern in
    let table = Array.make m 0 in
    for j = 1 to m - 1 do
      table.(j) <- assertInterrupted @@ search pattern m pattern j 0 1
    done;

    let rec loop table pattern m text n j k =
      if j = m then
        Found (k - j)
      else if k = n then
        Interrupted j
      else if pattern.[j] = text.[k] then
        loop table pattern m text n (j + 1) (k + 1)
      else if j = 0 then
        loop table pattern m text n 0 (k + 1)
      else
        loop table pattern m text n table.(j) k
    in

    loop table pattern m text n j k
  in

  global_search loop_search

(****************************************************)  
(*          Finally, making it efficient            *)  
(****************************************************)

let search_kmp =

  let loop_search pattern _ text n j k = 
    
    let rec loop table pattern m text n j k =
      if j = m then
        Found (k - j)
      else if k = n then
        Interrupted j
      else if pattern.[j] = text.[k] then
        loop table pattern m text n (j + 1) (k + 1)
      else if j = 0 then
        loop table pattern m text n 0 (k + 1)
      else
        loop table pattern m text n table.(j) k
    in
    let m = length pattern in
    let table = Array.make m 0 in

    (*  In the case of j = 1, j' is 0 *)
    for j = 2 to m - 1 do
      table.(j) <- assertInterrupted @@ 
        loop table pattern m pattern j table.(j - 1) (j - 1)
    done;
    loop table pattern m text n j k
  in

  global_search loop_search;;


let double_concat str = String.concat str [""; str];;

let cyc_rot str1 str2 =  let res1 = search_kmp (double_concat str1) str2 in
     match res1 with
     |Some x -> true
     | None -> false;;



cyc_rot "lenusya" "yalenus";;


let string_cycle_rotate s =
  let len = String.length s in
  let is_odd = len mod 2 == 1 in
  let hd = String.sub s 0 (len/2) in
  if is_odd
  then
    (let tl = String.sub s (len/2) (len/2 + 1) in
     String.concat "" [tl; hd])
  else
    (let tl = String.sub s (len/2) (len/2) in
     String.concat "" [tl; hd]);;

let x = ["abcde"; "eds"; "jidksaf"];;

let generate_words length num =
  let random_ascii_char _ = 
    let rnd = (Random.int 26) + 97 in
    Char.chr rnd
  in
  let random_string _ = 
    let buf = Buffer.create length in
    for _i = 0 to length - 1 do
      Buffer.add_char buf
        (random_ascii_char ())
    done;
    Buffer.contents buf
  in
  let acc = ref [] in
  for _i = 0 to num - 1 do
    acc := (random_string ()) :: ! acc
  done;
  !acc;;

(*
let generate_string_cycle_rotation n m =
  let words = generate_words n m in
  let cycled_words = List.map string_cycle_rotate words in
  let not_rotations =
    List.filter (fun non_rotations -> not (List.mem non_rotations words)) @@
      generate_words n m in
  (words, cycled_words, not_rotations);;
*)


let test_cycle n m =
  let words = ref (generate_words n m) in
  let rot_words = ref (List.map string_cycle_rotate !words) in
  let not_rot =
    ref (List.filter (fun non_rotations -> not (List.mem non_rotations !words)) @@
           generate_words n m) in
  for i = 0 to m - 2 do
    assert ((cyc_rot (List.hd !words) (List.hd !rot_words)) &&
         (not (cyc_rot (List.hd !words) (List.hd !not_rot))));
    words := List.tl !words;
    rot_words := List.tl !rot_words;
    not_rot := List.tl !not_rot;
  done;
  assert ((cyc_rot (List.hd !words) (List.hd !rot_words)) &&
            (not (cyc_rot (List.hd !words) (List.hd !not_rot))));
  true;;
