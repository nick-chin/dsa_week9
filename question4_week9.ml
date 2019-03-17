(* Implement a pattern search in a text, so it would explore the pattern right-to-left, but the main text left-to-right. Try to think of optimisations based on the characters in the pattern to speed-up your search and explain them in your report. Use the randomised automated tests from the lecture (or design new ones) to validate your implementation. *)

let left_right_search text pattern =
  let m = String.length pattern in
  let n = String.length text in
  (* text pointer k starts from m-1; since there can be no match in k[0,m-1] *)
  let k = ref (m-1) in
  (* pattern pointer j starts from the end of pattern *)
  let j = ref (m-1) in
  let res = ref None in
  let stop = ref false in
  while !k < n && not !stop do
    (*  Printf.printf "k = %d; j = %d\n" !k !j; *)
    if !j = 0
    then
      (res := Some !k;
       stop := true)
    else if pattern.[!j] <> text.[!k]
    then
      (k := !k + m - !j;
       (* reset j *)
       j := m-1)
    else
      (j := !j - 1;
       (* backtracking *)
       k := !k - 1)
  done;
  !res;;

(* Optimisations *)

(* Testing *)
let get_exn o = match o with
  | Some e -> e
  | _ -> raise (Failure "Empty result!");;

let test_pattern_in search text pattern =
  let index = get_exn @@ search text pattern in
  let p' = String.sub text index (String.length pattern) in
  assert (pattern = p');;

let test_pattern_not_in search text pattern =
  assert (search text pattern = None);;

let generate_words length num =
(* generates a random lowercase ASCII character *)
  let random_ascii_char _ =
    let rnd = (Random.int 26) + 97 in (* 97 corresponds to the character 'a' *)
    Char.chr rnd
  in
  (* creates a random string up to the given length *)
  let random_string _ =
    let buf = Buffer.create length in
    for i = 0 to length - 1 do
      Buffer.add_char buf (random_ascii_char ())
    done;
    Buffer.contents buf
  in
  let acc = ref [] in
  for i = 0 to num - 1 do
  (* adds string to accumulator *)
    acc := (random_string ()) :: ! acc
  done;
  !acc;;

let generate_string_and_patterns n m =
  let ps_in = generate_words n m in
  let ps_not_in =
    List.filter (fun p -> not (List.mem p ps_in)) @@
    generate_words n m in
  let s = String.concat "" (List.rev ps_in) in
  (s, ps_in, ps_not_in);;

let big = "abcdefghijklmnopeqrstuvabcsrtdsdqewgdcvaegbdweffwdajbjrag";;
let patterns = ["dsd"; "jrag"; "abc"];;

let search_tester search =
  let (s, ps, pn) = generate_string_and_patterns 500 5 in
  List.iter (fun p -> test_pattern_in search big p) patterns;
  List.iter (fun p -> test_pattern_in search s p) ps;
  List.iter (fun p -> test_pattern_not_in search s p) pn;
  true;;

search_tester left_right_search;;
