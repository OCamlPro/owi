let py_location = C_share_site.Sites.pyc

let bin_location = C_share_site.Sites.binc

let lib_location = C_share_site.Sites.libc

let find location file =
  List.find_map
    (fun dir ->
      let filename = Filename.concat dir file in
      if Sys.file_exists filename then Some filename else None )
    location

let get_libc () = find bin_location "libc.wasm"
