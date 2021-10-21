open Types

let datas _fmt _datas = assert false
let elems _fmt _elems = assert false
let start _fmt _start = assert false
let exports _fmt _exports = assert false
let globals _fmt _globals = assert false
let mems _fmt _mems = assert false
let tables _fmt _tables = assert false
let funcs _fmt _funcs = assert false
let imports _fmt _imports = assert false
let types _fmt _types = assert false

let module_fields fmt m =
  Format.fprintf fmt
    "%a@.%a@.%a@.%a@%a@%a@%a@%a@%a@%a@." types m.types imports m.imports funcs m.funcs tables m.tables mems m.mems globals m.globals exports m.exports start m.start elems m.elems datas m.datas

let id fmt = function
  | None -> ()
  | Some id ->
    Format.fprintf fmt "$%s" (String.init (List.length id) (fun i -> List.nth id i))

let module_ fmt m =
  Format.fprintf fmt "(module %a@.%a)" id m.id module_fields m
