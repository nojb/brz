let nullable : Re.t -> bool =
  let module M = Memoize.Make(Re) in
  M.fix
    (fun nullable r ->
       match Re.desc r with
       | Empty -> false
       | Epsilon -> true
       | Character _ -> false
       | Alternative (r1, r2) -> nullable r1 || nullable r2
       | Sequence (r1, r2) -> nullable r1 && nullable r2
       | Repeat _ -> true
    )

module Char = struct
  include Char
  let equal = (==)
  let hash = Hashtbl.hash
end

let deriv : char -> Re.t -> Re.t =
  let module C = Memoize.Make(Char) in
  let module M = Memoize.Make(Re) in
  C.memoize (fun c ->
      M.fix (fun deriv r ->
          match Re.desc r with
          | Empty | Epsilon ->
              Re.zero
          | Character c' ->
              if c = c' then Re.one else Re.zero
          | Alternative (r1, r2) ->
              Re.alter (deriv r1) (deriv r2)
          | Sequence (r1, r2) ->
              Re.alter (Re.seq (deriv r1) r2) (if nullable r1 then deriv r2 else Re.zero)
          | Repeat r1 ->
              Re.seq (deriv r1) r
        )
    )

let rec matches s i n r =
  if i = n then
    nullable r
  else
    matches s (i+1) n (deriv s.[i] r)

let matches s r =
  matches s 0 (String.length s) r
