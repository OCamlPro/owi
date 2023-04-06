open Lwt
open Cohttp_lwt_unix

let asset_loader path =
  match Content.read path with None -> assert false | Some asset -> asset

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let l = String.split_on_char '/' uri in
    let content, mime =
      if List.mem "life_browser.js" l then
        (asset_loader "life_browser.js", "application/javascript")
      else if List.mem "life.wasm" l then
        (asset_loader "life.wasm", "application/wasm")
      else (asset_loader "index.html", "text/html")
    in

    (body |> Cohttp_lwt.Body.to_string >|= fun _body -> content) >>= fun body ->
    let headers = Cohttp.Header.init () in
    let headers = Cohttp.Header.add headers "Content-Type" mime in
    Server.respond_string ~headers ~status:`OK ~body ()
  in

  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () =
  ignore @@ Sys.command "xdg-open http://localhost:8000";
  ignore (Lwt_main.run server);
  ()
