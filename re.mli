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

module Make(Char:CHAR)(String:STRING with type char = Char.t) : S with type char = Char.t and type string = String.t

module Bytes : S with type char = char and type string = string

module UTF8 : S with type char = Uchar.t and type string = string
