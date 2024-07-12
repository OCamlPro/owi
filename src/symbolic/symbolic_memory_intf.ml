module type S = sig
  type collection

  val init : unit -> collection

  val clone : collection -> collection
end
