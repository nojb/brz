type t

val pp: Format.formatter -> t -> unit
val zero: t
val one: t
val eps: t
val char: Uchar.t -> t
val string: string -> t
val disj: t list -> t
val conj: t list -> t
val compl: t -> t
val seq: t -> t -> t
val rep: t -> t
val rep1: t -> t
val opt: t -> t
val compare: t -> t -> int
val equal: t -> t -> bool
val hash: t -> int

val deriv: Uchar.t -> t -> t
val matches: t -> string -> bool
