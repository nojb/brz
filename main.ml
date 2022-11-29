module Brz = Brz.Re.UTF8

let rec seqn r = function
  | 0 -> Brz.one
  | n -> Brz.seq r (seqn r (pred n))

let bad r n =
  Brz.seq (seqn (Brz.disj [r; Brz.eps]) n) (seqn r n)

let test_bad n =
  let a = 'a' in
  let s = String.make n a in
  let r = bad (Brz.char (Uchar.of_char a)) n in
  let t0 = Unix.gettimeofday () in
  let ok = Brz.matches r s in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%4d\t%.2fs\t%B\n%!" n (t1 -. t0) ok

let () =
  List.iter test_bad [5; 10; 20; 30; 40; 50; 60; 100; 200]

module Test = struct
  open Re

  let test_bad n =
    let a = 'a' in
    let s = String.make n a in
    let r = seq [repn (opt (char a)) n (Some n); repn (char a) n (Some n)] in
    let r = Re.compile r in
    let t0 = Unix.gettimeofday () in
    let ok = Re.execp r s in
    let t1 = Unix.gettimeofday () in
    Printf.printf "%4d\t%.2fs\t%B\n%!" n (t1 -. t0) ok

  let () =
    List.iter test_bad [5; 10; 20; 30; 40; 50; 60; 100; 200]
end
