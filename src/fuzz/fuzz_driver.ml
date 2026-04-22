let pp_model ppf model =
  Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf " ; ") Concrete_value.pp ppf model

let rec run ~rounds f =
  match f () with
  | Ok () ->
    Fuzz_state.reset ();
    begin match rounds with
    | None -> run ~rounds f
    | Some 0 -> Ok ()
    | Some n -> run ~rounds:(Some (pred n)) f
    end
  | Error _ as e ->
    Log.app (fun m -> m "Found a bug with model: %a" pp_model !Fuzz_state.model);
    e
