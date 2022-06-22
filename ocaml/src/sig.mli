module type COMPARABLE = sig
  type t
  val equal : t -> t -> bool
end
