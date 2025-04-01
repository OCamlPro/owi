module S = Tiny_httpd

let asset_loader path =
  match Content.read path with None -> assert false | Some asset -> asset

let () =
  let server = S.create ~port:8000 () in
  S.add_route_handler ~meth:`GET server S.Route.return (fun _req ->
    S.Response.make_string
      ~headers:[ ("Content-Type", "text/html") ]
      (Ok (asset_loader "index.html")) );

  S.add_route_handler ~meth:`GET server
    S.Route.(exact "life_browser.js" @/ return)
    (fun _req ->
      S.Response.make_string
        ~headers:[ ("Content-Type", "application/javascript") ]
        (Ok (asset_loader "life_browser.js")) );

  S.add_route_handler ~meth:`GET server
    S.Route.(exact "life.wasm" @/ return)
    (fun _req ->
      S.Response.make_string
        ~headers:[ ("Content-Type", "application/wasm") ]
        (Ok (asset_loader "life.wasm")) );

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  (* "&": if web browser is closed, the command must be executed in background *)
  ignore @@ Sys.command "xdg-open http://localhost:8000 &";
  ignore (match S.run server with Ok () -> () | Error e -> raise e)
