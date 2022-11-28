type t =
  {
    desc: desc;
    id: int;
  }

and desc =
  | Empty
  | Epsilon
  | Character of char
  | Alternative of t * t
  | Sequence of t * t
  | Repeat of t

let desc t = t.desc

module Desc = struct
  type nonrec t = t

  let equal t1 t2 =
    match t1.desc, t2.desc with
    | Empty, Empty
    | Epsilon, Epsilon -> true
    | Character c1, Character c2 -> c1 == c2
    | Alternative (r1, r1'), Alternative (r2, r2')
    | Sequence (r1, r1'), Sequence (r2, r2') -> r1 == r2 && r1' == r2'
    | Repeat r1, Repeat r2 -> r1 == r2
    | _ -> false

  let hash t = Hashtbl.hash t.desc
end

module Hash = Weak.Make(Desc)

let last_id = ref 0

let sharing_tbl = Hash.create 1024

let mk desc =
  let id = incr last_id; !last_id in
  Hash.merge sharing_tbl {id; desc}

let zero = mk Empty

let one = mk Epsilon

let char c =
  mk (Character c)

let alter r1 r2 =
  match r1.desc, r2.desc with
  | Empty, _ -> r2
  | _, Empty -> r1
  | _ -> mk (Alternative (r1, r2))

let seq r1 r2 =
  match r1.desc, r2.desc with
  | Empty, _ | _, Empty -> zero
  | Epsilon, _ -> r2
  | _, Epsilon -> r1
  | _ -> mk (Sequence (r1, r2))

let rep r =
  match r.desc with
  | Empty | Epsilon -> one
  | _ -> mk (Repeat r)

let compare t1 t2 = Int.compare t1.id t2.id

let equal t1 t2 = Int.equal t1.id t2.id

let hash t = t.id
