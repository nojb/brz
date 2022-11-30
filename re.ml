module Re : sig
  type t

  and desc =
    | Epsilon
    | Character of Uchar.t
    | Alternative of t list
    | Conjunction of t list
    | Sequence of t * t
    | Repeat of t
    | Complement of t

  val mk: desc -> t

  val desc: t -> desc

  val equal: t -> t -> bool

  val compare: t -> t -> int

  val hash: t -> int
end = struct
  type t =
    {
      desc: desc;
      id: int;
    }

  and desc =
    | Epsilon
    | Character of Uchar.t
    | Alternative of t list
    | Conjunction of t list
    | Sequence of t * t
    | Repeat of t
    | Complement of t

  module Hashcons = Weak.Make(struct
      type nonrec t = t

      let equal t1 t2 =
        match t1.desc, t2.desc with
        | Epsilon, Epsilon -> true
        | Character c1, Character c2 -> Uchar.equal c1 c2
        | Alternative r1s, Alternative r2s
        | Conjunction r1s, Conjunction r2s -> List.equal (fun r1 r2 -> Int.equal r1.id r2.id) r1s r2s
        | Sequence (r1a, r2a), Sequence (r1b, r2b) -> Int.equal r1a.id r1b.id && Int.equal r2a.id r2b.id
        | Repeat r1, Repeat r2
        | Complement r1, Complement r2 -> Int.equal r1.id r2.id
        | _ -> false

      let hash t = Hashtbl.hash t.desc
    end)

  let last_id = ref 0

  let sharing_tbl = Hashcons.create 1024

  let mk desc =
    let id = incr last_id; !last_id in
    Hashcons.merge sharing_tbl {id; desc}

  let desc t = t.desc

  let equal t1 t2 = Int.equal t1.id t2.id

  let compare t1 t2 = Int.compare t1.id t2.id

  let hash t = t.id
end

include Re

let utf_8_to_string u =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf u;
  Buffer.contents buf

let rec pp ppf r =
  match desc r with
  | Epsilon -> Format.pp_print_string ppf "ε"
  | Character c -> Format.pp_print_string ppf (utf_8_to_string c)
  | Alternative [] -> Format.pp_print_string ppf "0"
  | Alternative rs ->
      Format.fprintf ppf "@[(%a)@]" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " + ") pp) rs
  | Conjunction [] -> Format.pp_print_string ppf "1"
  | Conjunction _ -> assert false
  | Sequence (r1, r2) -> Format.fprintf ppf "%a%a" pp r1 pp r2
  | Repeat r -> Format.fprintf ppf "@[(%a)*@]" pp r
  | Complement _ -> assert false

let zero = mk (Alternative [])

let one = mk (Conjunction [])

let eps = mk Epsilon

let char c = mk (Character c)

let compl r =
  match desc r with
  | Alternative [] -> one
  | Conjunction [] -> zero
  | Complement r -> r
  | _ -> mk (Complement r)

let sort_uniq rs =
  let rec insert r = function
    | [] -> [r]
    | x :: l as rs ->
        let c = compare r x in
        if c = 0 then rs
        else if c < 0 then r :: rs
        else x :: insert r l
  in
  List.fold_left (fun accu r -> insert r accu) [] rs

let disj rs =
  match List.concat_map (fun r -> match desc r with Alternative rs -> rs | Conjunction [] -> raise Exit | _ -> [r]) rs with
  | exception Exit ->
      one
  | rs ->
      begin match sort_uniq rs with
      | [] -> zero
      | [r] -> r
      | rs -> mk (Alternative rs)
      end

let conj rs =
  match List.concat_map (fun r -> match desc r with Conjunction rs -> rs | Alternative [] -> raise Exit | _ -> [r]) rs with
  | exception Exit ->
      zero
  | rs ->
      begin match sort_uniq rs with
      | [] -> one
      | [r] -> r
      | rs -> mk (Conjunction rs)
      end

let rec seq r1 r2 =
  match desc r1, desc r2 with
  | Alternative [], _ | _, Alternative [] -> zero
  | Epsilon, _ -> r2 | _, Epsilon -> r1
  | Sequence (r1a, r1b), _ -> seq r1a (seq r1b r2)
  | _ -> mk (Sequence (r1, r2))

let utf_8_fold_left f accu s =
  let rec loop accu pos =
    if pos >= String.length s then
      accu
    else
      let d = String.get_utf_8_uchar s pos in
      loop (f accu (Uchar.utf_decode_uchar d)) (pos + Uchar.utf_decode_length d)
  in
  loop accu 0

let string s =
  utf_8_fold_left (fun r c -> seq (char c) r) eps s

let rep r =
  match desc r with
  | Repeat _ | Epsilon -> r
  | Alternative [] -> eps
  | Conjunction [] -> one
  | _ -> mk (Repeat r)

let rep1 r =
  seq r (rep r)

let opt r =
  disj [r; eps]

let nullable : t -> bool =
  let module M = Memoize.Make(Re) in
  M.fix
    (fun nullable r ->
       match desc r with
       | Epsilon -> true
       | Character _ -> false
       | Alternative rs -> List.exists nullable rs
       | Conjunction rs -> List.for_all nullable rs
       | Sequence (r1, r2) -> nullable r1 && nullable r2
       | Repeat _ -> true
       | Complement r -> not (nullable r)
    )

let deriv : Uchar.t -> t -> t =
  let module C = Memoize.Make(Uchar) in
  let module M = Memoize.Make(Re) in
  C.memoize (fun c ->
      M.fix (fun deriv r ->
          match Re.desc r with
          | Epsilon ->
              zero
          | Character c' ->
              if c = c' then eps else zero
          | Alternative rs ->
              disj (List.map deriv rs)
          | Conjunction rs ->
              conj (List.map deriv rs)
          | Sequence (r1, r2) ->
              if nullable r1 then
                disj [seq (deriv r1) r2; deriv r2]
              else
                seq (deriv r1) r2
          | Repeat r1 ->
              seq (deriv r1) r
          | Complement r ->
              compl (deriv r)
        )
    )

let nonempty : t -> bool =
  let module M = Memoize.Make(Re) in
  M.fix (fun nonempty r ->
      match desc r with
      | Epsilon -> true
      | Character _ -> true
      | Alternative rs -> List.exists nonempty rs
      | Conjunction [] -> true
      | Conjunction _ -> assert false
      | Sequence (r1, r2) -> nonempty r1 && nonempty r2
      | Repeat _ -> true
      | Complement r -> not (nonempty r)
    )

let matches r s =
  match
    utf_8_fold_left (fun r c ->
        if nonempty r then deriv c r else raise Exit
      ) r s
  with
  | r -> nullable r
  | exception Exit -> false

(* module ByteChar = struct *)
(*   include Char *)
(*   let hash = Char.code *)
(*   let to_string = String.make 1 *)
(* end *)

(* module ByteString = struct *)
(*   type char = Char.t *)
(*   include String *)
(* end *)

(* module Bytes = Make(ByteChar)(ByteString) *)

(* module UTF8String = struct *)
(*   type char = Uchar.t *)
(*   type t = string *)
(*   let fold_left f accu s = *)
(*     let rec loop accu pos = *)
(*       if pos >= String.length s then *)
(*         accu *)
(*       else *)
(*         let d = String.get_utf_8_uchar s pos in *)
(*         loop (f accu (Uchar.utf_decode_uchar d)) (pos + Uchar.utf_decode_length d) *)
(*     in *)
(*     loop accu 0 *)
(* end *)

(* module Uchar = struct *)
(*   include Uchar *)

(*   let to_string u = *)
(*     let buf = Buffer.create 4 in *)
(*     Buffer.add_utf_8_uchar buf u; *)
(*     Buffer.contents buf *)
(* end *)

(* module UTF8 = Make(Uchar)(UTF8String) *)
