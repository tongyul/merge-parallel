signature BINSEARCH =
sig
  type t
  type 'a seq
  val firstGeq : t seq -> t -> int
  val firstGt : t seq -> t -> int
  val lastLeq : t seq -> t -> int
  val lastLt : t seq -> t -> int
end
