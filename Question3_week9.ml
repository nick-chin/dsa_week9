
(*Required Helper functions from Class*)

let out = ref false


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

let generate_words length num =
  let random_ascii_char _ =
    let rnd = (Random.int 26) + 97 in
    Char.chr rnd
  in
  let random_string _ =
    let buf = Buffer.create length in
    for i = 0 to length - 1 do
      Buffer.add_char buf (random_ascii_char ())
    done;
    Buffer.contents buf
  in
  let acc = ref [] in
  for i = 0 to num - 1 do
    acc := (random_string ()) :: ! acc
  done;
  !acc;;


(* Our answer for Question 3*)

let double_concat str = String.concat str [""; str];;

let cyclic_rotation str1 str2 = if str1 = str2 then false else let res1 = search_kmp (double_concat str1) str2 in
     match res1 with
     |Some x -> if (String.length str2 = String.length str1) then true else false
     | None -> false;;


cyclic_rotation  "yalenus" "lenusya";;

(*Test*)


let flip lst = let head = List.hd (List.rev lst) in
head :: (List.rev (List.tl (List.rev lst)));;

let truetest1 func = let str_list = generate_words 5 10 in
 let word = String.concat "" str_list in
 let word_in_reverse = String.concat "" (flip str_list) in
 func word word_in_reverse;;

let%test "Should return true for cyclic rotation" =
  truetest1 cyclic_rotation = true;;








