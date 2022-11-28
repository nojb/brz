module Make (H : Hashtbl.HashedType) : sig
  val memoize: (H.t -> 'a) -> (H.t -> 'a)
  val fix: ((H.t -> 'a) -> H.t -> 'a) -> (H.t -> 'a)
end
