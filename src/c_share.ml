let py_location = List.map Fpath.v C_share_site.Sites.pyc

let bin_location = List.map Fpath.v C_share_site.Sites.binc

let lib_location = List.map Fpath.v C_share_site.Sites.libc

let find location file =
  List.find_map
    (fun dir ->
      let filename = Fpath.append dir file in
      match Bos.OS.File.exists filename with
      | Ok true -> Some filename
      | Ok false -> None
      | Error (`Msg msg) -> failwith msg )
    location

let libc = Option.get @@ find bin_location (Fpath.v "libc.wasm")
