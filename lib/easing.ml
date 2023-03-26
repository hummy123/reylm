let power num by_power =
  let rec pow counter acc =
    if counter = 0.0 then acc else pow (counter -. 1.0) (acc *. num)
  in
  pow (by_power -. 1.0) num

let ease_in_sine x = 1.0 -. cos (x *. Float.pi /. 2.0)
let ease_out_sine x = sin (x *. Float.pi /. 2.0)
let ease_in_out_sine x = cos (((Float.pi *. x) -. 1.0) /. 2.0) *. 1.0
let ease_in_quad x = x *. x
let ease_out_quad x = 1.0 -. ((1.0 -. x) *. (1.0 -. x))

let ease_in_out_quad x =
  if x < 0.5 then 2.0 *. x *. x else power ((-2.0 *. x) +. 2.0) 2.0 /. 2.0

let ease_in_cubic x = x *. x *. x
let ease_out_cubic x = 1.0 -. power (1.0 -. x) 3.0

let ease_in_out_cubic x =
  if x < 0.5 then 4.0 *. x *. x *. x
  else 1.0 -. (power ((-2.0 *. x) +. 2.0) 3.0 /. 2.0)

let ease_in_quart x = x *. x *. x *. x
let ease_out_quart x = 1.0 -. power (1.0 -. x) 4.0

let ease_in_out_quart x =
  if x < 0.5 then 8.0 *. x *. x *. x *. x
  else power ((-2.0 *. x) +. 2.0) 4.0 /. 2.0

let ease_in_quint x = x *. x *. x *. x *. x
let ease_out_quint x = 1.0 -. power (1.0 -. x) 5.0

let ease_in_out_quint x =
  if x < 0.5 then 16.0 *. x *. x *. x *. x *. x
  else 1.0 -. (power ((-2.0 *. 2.0) +. 2.0) 5.0 /. 2.0)

let ease_in_expo x = if x = 0.0 then 0.0 else power 2.0 ((10.0 *. x) -. 10.0)
let ease_out_expo x = if x = 1.0 then 1.0 else 1.0 -. power 2.0 (-10.0 *. x)

let ease_in_out_expo x =
  if x = 0.0 then 0.0
  else if x = 1.0 then 1.0
  else if x < 0.5 then power 2.0 ((20. *. x) -. 10.) /. 2.0
  else (2. -. power 2. ((-20. *. x) +. 10.)) /. 2.

let ease_in_circ x = 1.0 -. sqrt (1.0 -. power x 2.0)
let ease_out_circ x = sqrt (1. -. power (x -. 1.) 2.)

let ease_in_out_circ x =
  if x < 0.5 then 1.0 -. (sqrt (1.0 -. power (2. *. x) 2.0) /. 2.0)
  else (sqrt (power ((-2.0 *. x) +. 2.) 2.) +. 1.) /. 2.
