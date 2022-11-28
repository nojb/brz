type t

type desc =
  | Empty
  | Epsilon
  | Character of char
  | Alternative of t * t
  | Sequence of t * t
  | Repeat of t

val desc: t -> desc
val zero: t
val one: t
val char: char -> t
val alter: t -> t -> t
val seq: t -> t -> t
val rep: t -> t
val compare: t -> t -> int
val equal: t -> t -> bool
val hash: t -> int
