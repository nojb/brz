module Brz = Brz.Re

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

let len = 1024 * 1024

let s = String.init len (fun i -> if i = len - 1 then 'b' else 'a')

let () =
  let a = Uchar.of_char 'a' in
  let b = Uchar.of_char 'b' in
  let r = Brz.(seq one (seq (char a) (seq (opt (char a)) (char b)))) in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to 100 do
    assert (Brz.matches r s)
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "Brz %.2fs\n%!" (t1 -. t0)

let () =
  let open Re in
  let a = 'a' in
  let b = 'b' in
  let r = seq [char a; opt (char a); char b] in
  let r = compile r in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to 100 do
    assert (Re.execp r s)
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "Re %.2fs\n%!" (t1 -. t0)
