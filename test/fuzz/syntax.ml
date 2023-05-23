let ( let* ) = Crowbarplus.dynamic_bind

let ( let+ ) gen map_fn = Crowbarplus.map [ gen ] map_fn

let ( and+ ) = Crowbarplus.pair
