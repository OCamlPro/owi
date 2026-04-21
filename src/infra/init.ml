let random_state =
  let init = ref false in
  fun seed ->
    if not !init then begin
      let seed = Option.value seed ~default:42 in
      Random.init seed
    end
    else begin
      Logs.warn (fun m ->
        m "The random state initialization function was called too many times" )
    end
