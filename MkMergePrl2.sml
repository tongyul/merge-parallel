functor MkMergePrl2 (structure Seq : SEQUENCE structure Ord : ORDERED val ts : Ord.t -> string) :> MERGE
where type t = Ord.t
  and type 'a seq = 'a Seq.t
=
struct

  type t = Ord.t
  type 'a seq = 'a Seq.t

  val cmp = Ord.compare

  val sts = Seq.toString ts

  fun merge (s1 : t seq,s2 : t seq) : t seq =
  let
    (* val () = print ("merge " ^ sts s1 ^ " " ^ sts s2 ^ "\n") *)
    val n1 = Seq.length s1
    val n2 = Seq.length s2
    val m1 = Seq.length s1 div 2
    val m2 = Seq.length s2 div 2
    val nth1 = Seq.nth s1
    val nth2 = Seq.nth s2
    val ss1 = Seq.subseq s1
    val ss2 = Seq.subseq s2
  in
    if n1 = 0 then
      s2
    else if n2 = 0 then
      s1
    else if cmp (nth1 (n1 - 1),nth2 0) <> GREATER then
      Seq.append (s1,s2)
    else if cmp (nth2 (n2 - 1),nth1 0) <> GREATER then
      Seq.append (s2,s1)
    else if cmp (nth1 m1,nth2 0) <> GREATER then
      Seq.append (ss1 (0,m1 + 1),merge (ss1 (m1 + 1,n1 - m1 - 1),s2))
    else if cmp (nth1 m1,nth2 (n2 - 1)) <> LESS then
      Seq.append (merge (ss1 (0,m1),s2),ss1 (m1,n1 - m1))
    else if cmp (nth2 m2,nth1 0) <> GREATER then
      Seq.append (ss2 (0,m2 + 1),merge (s1,ss2 (m2 + 1,n2 - m2 - 1)))
    else if cmp (nth2 m2,nth1 (n1 - 1)) <> LESS then
      Seq.append (merge (s1,ss2 (0,m2)),ss2 (m2,n2 - m2))
    else
      let
        val lTask = fn () => merge (ss1 (0,m1),ss2 (0,m2))
        val rTask = fn () => merge (ss1 (m1,n1 - m1),ss2 (m2,n2 - m2))
        val (l,r) = Primitives.par (lTask,rTask)
        val ssL = Seq.subseq l
        val ssR = Seq.subseq r
      in
        case cmp (nth1 m1,nth2 m2) of
          EQUAL => Seq.append (l,r)
        | LESS => Seq.flatten (Seq.fromList [ssL (0,m1),merge (ssL (m1,m2),ssR (0,n1 - m1)),ssR (n1 - m1,n2 - m2)])
        | GREATER => Seq.flatten (Seq.fromList [ssL (0,m2),merge (ssL (m2,m1),ssR (0,n2 - m2)),ssR (n2 - m2,n1 - m1)])
      end
  end

end
