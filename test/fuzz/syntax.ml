let ( let* ) = Crowbar.dynamic_bind

let ( let+ ) gen map_fn = Crowbar.map [ gen ] map_fn

let ( and+ ) = Crowbar.pair
