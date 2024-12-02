module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  val zero: t
  val one: t
  val i : t
  val neg: t -> t
  val konj: t -> t
  val ( ++ ): t -> t -> t
  val ( -- ): t -> t -> t
  val ( ** ): t -> t -> t
  val scalar: float -> t -> t
end

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = if (x.re = y.re && x.im = y.im) then true else false
  let zero = {re = 0.; im = 0.}
  let one = {re = 1.; im = 0.}
  let i = {re=0.; im=1.}
  let neg x = {re = -.x.re; im = -.x.im}
  let konj x = {re = x.re; im = -.x.im}
  let ( ++ ) x y = {re = x.re +. y.re; im = x.im +. y.im}
  let ( -- ) x y = {re = x.re -. y.re; im = x.im -. y.im}
  let ( ** ) x y = {re = x.re *. y.re -. x.im *. y.im; im = x.im *. y.re +. x.re *. y.im}
  let scalar x y = y

end

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.
  let eq x y = if ((x.magn = y.magn) && (x.arg = y.arg) ) then true else false
  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = 90.}
  let neg x = {magn = x.magn; arg = (x.arg +. pi)}
  let konj x = {magn = x.magn; arg = (-. x.arg)}
  let ( ++ ) (x) (y) =
  let xab =  x.magn *. (cos (rad_of_deg x.arg)) +. y.magn *. cos (rad_of_deg y.arg) in
  let yab = (x.magn *. (sin (rad_of_deg x.arg))) +. y.magn *. sin (rad_of_deg y.arg) in
  let rab = sqrt (xab *. xab +. yab *. yab) in
  let phiab = Float.atan ((x.magn *. (sin (rad_of_deg x.arg)) +. y.magn *. sin (rad_of_deg y.arg)) /. ((x.magn *. (cos (rad_of_deg x.arg))) +. y.magn *. cos (rad_of_deg y.arg)))
  in {magn = rab; arg = phiab}
  
  let ( -- ) x y =
  let my = neg y in
   (++) x my
  
  let ( ** ) x y = {magn = x.magn *. y.magn; arg = x.arg +. y.arg}
  let scalar x c = {c with magn = x *. c.magn}

end

open Polar
(* let a = let open Polar in {magn = 10. ; arg = 90.} *)
(* let b = {magn = 3.5 ; arg = 230.}
let f = Polar.(++) a b  *)