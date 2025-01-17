let generate_word length =
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
  random_string();;


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
    Hashtbl.hash @@ String.mapi (fun i c -> if i mod 2 = 0 then c else ' ') s
  let hash3 s = Hashtbl.hash @@ String.concat "" [s; s]
  let hash_functions = [hash1; hash2; hash3]
end




module URLGenerator = struct
  module SF = BloomFilterImpl(StringHashing)
  open SF

  let filter =
    let f = mk_bloom_filter 1000 in
    f

  let generate_fresh_url _ =
    let url = ref (generate_word 10) in
    while contains filter !url do
      url := generate_word 10
    done;
    insert filter !url;
    String.concat "" ["url.com/"; !url]

end

module Gen = URLGenerator;;
open Gen;;

Gen.filter;;
Gen.SF.print_filter Gen.filter;;
Gen.generate_fresh_url ();;
Gen.SF.print_filter Gen.filter;;
Gen.SF.contains Gen.filter "obghtagajq";;
Gen.generate_fresh_url ();;
Gen.generate_fresh_url ();;
Gen.generate_fresh_url ();;
Gen.generate_fresh_url ();;
Gen.SF.print_filter Gen.filter;;
