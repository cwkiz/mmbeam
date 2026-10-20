type t =
  | Small of int
  | Big of Z.t

let of_z z =
  if Z.fits_int z then Small (Z.to_int z) else Big z

let to_z = function
  | Small n -> Z.of_int n
  | Big n -> n

let of_int n = Small n

let add left right = of_z Z.(add (to_z left) (to_z right))

let sub left right = of_z Z.(sub (to_z left) (to_z right))
