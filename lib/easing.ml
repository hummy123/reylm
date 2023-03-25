let power num by_power =
  let rec pow counter acc =
    if counter = 0.0 then acc else pow (counter -. 1.0) (acc *. num)
  in
  pow (by_power -. 1.0) num

let ease_out_circ x =
  let x = power (x -. 1.0) 2.0 in
  let x = 1.0 -. x in
  sqrt x
