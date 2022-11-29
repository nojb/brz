module type STRING = sig
  type t
  type char
  val fold_left: ('a -> char -> 'a) -> 'a -> t -> 'a
end

module type CHAR = sig
  type t
  val equal: t -> t -> bool
  val hash: t -> int
end

module type S = sig
  type t
  type char
  type string

  val zero: t
  val one: t
  val eps: t
  val char: char -> t
  val string: string -> t
  val disj: t list -> t
  val conj: t list -> t
  val compl: t -> t
  val seq: t -> t -> t
  val rep: t -> t
  val rep1: t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int

  val matches: t -> string -> bool
end

module Make(Char:CHAR)(String:STRING with type char = Char.t) = struct
  type char = Char.t
  type string = String.t

  module Re : sig
    type t

    and desc =
      | Epsilon
      | Character of Char.t
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
      | Character of Char.t
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
          | Character c1, Character c2 -> Char.equal c1 c2
          | Alternative r1s, Alternative r2s
          | Conjunction r1s, Conjunction r2s -> List.equal (fun r1 r2 -> Int.equal r1.id r2.id) r1s r2s
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
    match List.concat_map (fun r -> match desc r with Alternative [] -> raise Exit | Conjunction rs -> rs | _ -> [r]) rs with
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

  let string s =
    String.fold_left (fun r c -> seq (char c) r) eps s

  let rep r =
    match desc r with
    | Repeat _ | Epsilon -> r
    | Alternative [] -> eps
    | Conjunction [] -> one
    | _ -> mk (Repeat r)

  let rep1 r =
    seq r (rep r)

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

  let deriv : Char.t -> t -> t =
    let module C = Memoize.Make(Char) in
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

  let matches r s =
    nullable (String.fold_left (fun r c -> deriv c r) r s)
end

module ByteChar = struct
  include Char
  let hash = Char.code
end

module ByteString = struct
  type char = Char.t
  include String
end

module Bytes = Make(ByteChar)(ByteString)

module UTF8String = struct
  type char = Uchar.t
  type t = string
  let fold_left f accu s =
    let rec loop accu pos =
      if pos >= String.length s then
        accu
      else
        let d = String.get_utf_8_uchar s pos in
        loop (f accu (Uchar.utf_decode_uchar d)) (pos + Uchar.utf_decode_length d)
    in
    loop accu 0
end

module UTF8 = Make(Uchar)(UTF8String)
