(* Exercise 2 *)

open String

let naive_search_all text pattern =
let n = String.length text in
let m = String.length pattern in
if n < m then []
else
  let k = ref 0 in (* k points to the start index of the text *)
  let res = ref [] in
  while !k <= n - m do
    let j = ref 0 in (* j points to the index in the pattern *)
    while !j <= m - 1 && text.[!k + !j] = pattern.[!j]
    do  j := !j + 1  done;
    if !j = m then res := Some !k :: !res;
    k := !k + 1
  done;
  List.rev !res

let rk_hash text =
  let h = ref 0 in
  for i = 0 to String.length text - 1 do
    h := !h + Char.code text.[i]
  done;
  !h

let rabin_karp_search_all text pattern =
  let n = String.length text in
  let m = String.length pattern in
  if n < m then []
  else
    (* Compute as the sum of all characters in pattern *)
    let hpattern = rk_hash pattern in
    let rolling_hash = ref @@ rk_hash (String.sub text 0 m) in
    let i = ref 0 in
    let res = ref [] in
    while !i <= n - m do
      (if hpattern = !rolling_hash &&
          String.sub text !i m = pattern then
        res := Some !i :: !res);
      (* Update the hash *)
      (if !i <= n - m - 1
        then
          let c1 = Char.code text.[!i] in
          let c2 = Char.code text.[!i + m] in
          rolling_hash := !rolling_hash - c1 + c2);
      i := !i + 1
    done;
    List.rev !res

let generate_words length num =
  let random_ascii_char _ = 
    let rnd = (Random.int 26) + 97 in
    Char.chr rnd
  in
  let random_string _ = 
    let buf = Buffer.create length in
    for _i = 0 to length - 1 do
      Buffer.add_char buf (random_ascii_char ())
    done;
    Buffer.contents buf
  in
  let acc = ref [] in
  for _i = 0 to num - 1 do
    acc := (random_string ()) :: ! acc
  done;
  !acc

let generate_string_and_patterns n m =
  let ps_in = generate_words n m in
  let ps_not_in =
    List.filter (fun p -> not (List.mem p ps_in)) @@
    generate_words n m in
  let s = String.concat "" (List.rev ps_in) in
  let s' = String.concat "" (s :: List.rev ps_in) in
  (s', ps_in, ps_not_in)

let get_exn o = match o with
| Some e -> e
| _ -> raise (Failure "Empty option!") 

let test_pattern_not_in search text pattern =
  assert (search text pattern = [])

let test_pattern_in search text pattern =
  let ls = search text pattern in 
  assert (List.length ls = 2);
  let test_pattern_in_aux e =
  let p' = String.sub text (get_exn e) (String.length pattern) in 
  assert (pattern = p') in
  List.iter (fun e -> test_pattern_in_aux e) ls
 
let search_tester search =
  let (s, ps, pn) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_in search s p) ps;
  List.iter (fun p -> test_pattern_not_in search s p) pn;
  true