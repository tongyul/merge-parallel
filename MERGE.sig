signature MERGE =
sig
  type t
  type 'a seq
  (* assumes that inputs are sorted;
   * gives back sorted sequence with orginal elements *)
  val merge: t seq * t seq -> t seq
end
