let rec seqn r = function
  | 0 -> Re.one
  | n -> Re.seq r (seqn r (pred n))

let bad r n =
  Re.seq (seqn (Re.alter r Re.one) n) (seqn r n)

let test_bad n =
  let a = 'a' in
  let s = String.make n a in
  let r = bad (Re.char a) n in
  let t0 = Unix.gettimeofday () in
  let ok = Deriv.matches s r in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%4d\t%.2fs\t%B\n%!" n (t1 -. t0) ok

let () =
  List.iter test_bad [5; 10; 20; 30; 40; 50; 60; 100; 200]
