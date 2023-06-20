module type Env = sig
  type t

  type memory
  type func
  type table
  type elem
  type data
  type global

  module Value : Value_intf.T
end
