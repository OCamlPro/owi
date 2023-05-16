include Crowbar

(* TODO: PR to crowbar with this function ? *)
let list_append l1 l2 = map [ l1; l2 ] List.append

let list_cons hd tl = map [ hd; tl ] List.cons
