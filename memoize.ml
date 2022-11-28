module Make(H : Hashtbl.HashedType) = struct
  module Hash = Hashtbl.Make(H)

  let memoize f =
    let memo = Hash.create 1024 in
    fun x ->
      try
        Hash.find memo x
      with Not_found ->
        let y = f x in
        Hash.add memo x y;
        y

  let fix f =
    let memo = Hash.create 1024 in
    let rec f' x =
      try
        Hash.find memo x
      with Not_found ->
        let y = f f' x in
        Hash.add memo x y;
        y
    in
    f'
end
