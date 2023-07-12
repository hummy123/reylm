open Eio.Std

type 'model callback =
  (* Then = terminating closure that updates a model and has no more work to do. *)
  | Model of ('model -> 'model)
  (* Then = closure that is run in async. *)
  | Then of ('model -> 'model callback)
  (* ModelThen = a function that updates a model, and then runs a closure in async afterwards. *)
  | ModelThen of ('model -> 'model) * ('model -> 'model callback)

type 'a t = 'a list
type ('model, 'callback) apply_result = { model : 'model; lst : 'callback list }

let empty = []

let create_promise model sw callback =
  let promise, resolver = Promise.create () in
  Fiber.fork ~sw (fun () ->
      let next = callback model in
      Promise.resolve resolver next);
  promise

let append model sw callback lst =
  match callback with
  | Model callback -> { model = callback model; lst }
  | Then async_callback ->
      let promise = create_promise model sw async_callback in
      { model; lst = promise :: lst }
  | ModelThen (model_callback, async_callback) ->
      let model = model_callback model in
      let promise = create_promise model sw async_callback in
      { model; lst = promise :: lst }

let rec apply model sw lst =
  match lst with
  | [] -> { model; lst }
  | head :: _ -> (
      let { model; lst } = apply model sw lst in
      match Promise.peek head with
      | Some x -> (
          match x with
          | Model f -> { model = f model; lst }
          | Then f ->
              let promise = create_promise model sw f in
              { model; lst = promise :: lst }
          | ModelThen (model_callback, async_callback) ->
              let model = model_callback model in
              let promise = create_promise model sw async_callback in
              { model; lst = promise :: lst })
      | None -> { model; lst = head :: lst })
