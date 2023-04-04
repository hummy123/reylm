type label = string

type state =
  | Button of Button_state.button_state
  | ListView of Listview_state.listview_state

type state_tree = SE | ST of int * state_tree * label * state * state_tree

(* AVL Tree balancing functions (not inteeresting to domain). *)
let ht = function SE -> 0 | ST (h, _, _, _, _) -> h

let mk lt lb st rt =
  let lh = ht lt in
  let rh = ht rt in
  let h = (if lh > rh then lh else rh) + 1 in
  ST (h, lt, lb, st, rt)

let bal_L ab xl xs c =
  if ht ab = ht c + 2 then
    match ab with
    | ST (_, a, yl, ys, b) -> (
        if ht a >= ht b then mk a yl ys (mk b xl xs c)
        else
          match b with
          | ST (_, b1, bxl, bxs, b2) ->
              mk (mk a yl ys b1) bxl bxs (mk b2 xl xs c)
          | x -> x)
    | x -> x
  else mk ab xl xs c

let bal_R a xl xs bc =
  if ht bc = ht a + 2 then
    match bc with
    | ST (_, b, yl, ys, c) -> (
        if ht b <= ht c then mk (mk a xl xs b) yl ys c
        else
          match b with
          | ST (_, b1, bxl, bxs, b2) ->
              mk (mk a xl xs b1) bxl bxs (mk b2 yl ys c)
          | x -> x)
    | x -> x
  else mk a xl xs bc

(* Interesting functions below: add label/state and find state by label. *)
let rec add label state = function
  | SE -> mk SE label state SE
  | ST (_, l, k, v, r) ->
      if label < k then bal_L (add label state l) k v r
      else if label > k then bal_R l k v (add label state r)
      else (* Update node with newly given state. *)
        mk l label state r

let rec find_opt key = function
  | SE -> None
  | ST (_, l, k, v, r) ->
      if key < k then find_opt key l
      else if key > k then find_opt key r
      else Some v

let rec draw_labels = function
  | SE -> ()
  | ST (_, l, sl, _, r) ->
      draw_labels l;
      Raylib.draw_text sl 20 20 20 Raylib.Color.black;
      draw_labels r
