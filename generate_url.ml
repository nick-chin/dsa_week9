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
  !acc



module ArrayPrinter = functor (P : sig
    type t
    val pp : t -> string
  end) -> struct

    (* Printing machinery *)
    let print_sub_array l u arr =
      assert (l <= u);
      assert (u <= Array.length arr);
      Printf.printf "[| ";
      for i = l to u - 1 do
        Printf.printf "%s" (P.pp arr.(i));
        if i < u - 1
        then Printf.printf "; "
        else ()      
      done;
      Printf.printf " |] "
        
    let print_array arr = 
      let len = Array.length arr in
      print_sub_array 0 len arr              
  end


                      
module type BloomHashing = sig
  type t
  val hash_functions : (t -> int) list  
end

(* Bloom filter signature *)
module type BloomFilter = functor
  (H: BloomHashing) -> sig
  type t
  val mk_bloom_filter : int -> t
  val insert : t -> H.t -> unit
  val contains : t -> H.t -> bool
  val print_filter : t -> unit
end


                     
(* Bloom filter implementation *)
module BloomFilterImpl : BloomFilter = functor
  (H: BloomHashing) -> struct

  (* Type of filter *)
  type t = {
    slots : bool array;
    size  : int
  }

  let mk_bloom_filter n = 
    let a = Array.make n false in
    {slots = a; size = n}

  let insert f e = 
    let n = f.size in
    List.iter (fun hash ->
        let h = (hash e) mod n in
        f.slots.(h) <- true) H.hash_functions

  let contains f e = 
    if H.hash_functions = [] then false
    else
      let n = f.size in
      let res = ref true in
      List.iter (fun hash ->
          let h = (hash e) mod n in
          res := !res && f.slots.(h)) H.hash_functions;
      !res
        
  module BP = ArrayPrinter(struct
      type t = bool
      let pp b = if b then "1" else "0"
    end)

  let print_filter t = 
    let open BP in
    print_array t.slots
    
end


module StringHashing = struct
  type t = string
  let hash1 s = Hashtbl.hash s
  let hash2 s =
    let rec rev_string s ls i =
      if List.length ls = String.length s
      then String.concat ""
      else rev_string s ((String.sub s i 1) :: ls) (i + 1)
    in
    Hashtbl.hash @@ rev_string s [] 0
  let hash3 s = Hashtbl.hash @@ String.concat "" [s; s]
  let hash_functions = [hash1; hash2; hash3]
end



module SF = BloomFilterImpl(StringHashing);;
open SF;;


module type Generator =
  sig
    val filter : t
    val generate_fresh_url : unit -> string
  end

module URLGenerator : Generator = struct
  module SF = BloomFilterImpl(StringHashing)
  open SF

  let filter =
    let f = mk_bloom_filter 1000 in
    f

  let generate_fresh_url _ =
    let url = ref (List.hd (generate_words 6 1)) in
    while contains filter !url do
      url := List.hd (generate_words 6 1)
    done;
    insert filter !url;
    String.concat "" ["url.com/"; !url]

end

module Gen = URLGenerator;;
open Gen;;
Gen.filter;;
print_filter Gen.filter;;
Gen.generate_fresh_url ();;
contains Gen.filter "obghta";;
