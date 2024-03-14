module type STORABLE = sig
  type t
  val withdraw : t -> bool
  val id : t -> int
end

module type STORE = sig
  type t
  val add : t -> unit
  val active_store : unit -> t list
end

module MakeStore : functor (Storable : STORABLE) -> STORE with type t = Storable.t

