functor MkMergePrl (structure Seq : SEQUENCE structure Ord : ORDERED val ts : Ord.t -> string) :> MERGE
where type t = Ord.t
  and type 'a seq = 'a Seq.t
=
struct

  type t = Ord.t
  type 'a seq = 'a Seq.t

  val sts = Seq.toString ts

  structure B = MkBinSearch (structure Seq = Seq structure Ord = Ord)

  fun partitionByCmp (s : t seq, x : t) : t seq * t seq * t seq =
  let
    val j = B.firstGeq s x
    val k = B.firstGt s x
    val n = Seq.length s
    val ss = Seq.subseq s
  in
    (ss (0, j), ss (j, k - j), ss (k, n - k))
  end

  fun merge (s1 : t seq, s2 : t seq) : t seq =
  let
    (* val () = *)
    (*   if Seq.length s1 <> 0 andalso Seq.length s2 <> 0 then *)
    (*     print ("merge " ^ sts s1 ^ " " ^ sts s2 ^ "\n") *)
    (*   else *)
    (*     () *)
    val n = Seq.length s1
  in
    if n = 0 then
      s2
    else
      let
        val m = Seq.nth s1 (n div 2)
        val (l1, e1, r1) = partitionByCmp (s1, m)
        val (l2, e2, r2) = partitionByCmp (s2, m)
        val lTask = fn () => merge (l2, l1)
        val rTask = fn () => merge (r2, r1)
        val (l, r) = Primitives.par (lTask, rTask)
      in
        Seq.flatten (Seq.fromList [l, e1, e2, r])
      end
  end

end
