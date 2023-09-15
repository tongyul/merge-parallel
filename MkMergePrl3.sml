functor MkMergePrl3 (structure Seq : SEQUENCE structure Ord : ORDERED val ts : Ord.t -> string) : MERGE
where type t = Ord.t
  and type 'a seq = 'a Seq.t
=
struct

  type t = Ord.t
  type 'a seq = 'a Seq.t

  (* Aliases of Seq functions *)
  val len = Seq.length
  val nth = Seq.nth
  val ss = Seq.subseq
  val take = Seq.take
  val drop = Seq.drop
  fun first s = nth s 0
  fun last s = nth s (len s - 1)

  (* Aliases of Ord functions *)
  (* vvv stable sorting requires s1 to win tie-breaking *)
  fun lt (x1,x2) = Ord.compare (x1,x2) <> GREATER
  fun gt (x1,x2) = Ord.compare (x1,x2) = GREATER

  (* Other aliases *)
  val par = Primitives.par
  val sts = Seq.toString ts

  exception Unreachable

  datatype frag =
      Empty
    | Slice of t seq (* should never be empty *)
    | Split of frag * frag * int * bool (* f1 f2 offset overlap *)

  fun fragLength Empty = 0
    | fragLength (Slice s) = Seq.length s
    | fragLength (Split (_,g,offset,_)) = offset + fragLength g

  fun writeSlice dest (s,offset) =
    (ignore o Seq.map (fn (x,d) => d := SOME x) o Seq.zip)
    (s,Seq.drop dest offset)

  fun writeFrag dest =
  let
    val ws = writeSlice dest
    fun wf (Empty,_,_) = ()
      | wf (Slice s,offset,dropTail) =
        if dropTail then
          ws (Seq.take s (Seq.length s - 1),offset)
        else
          ws (s,offset)
      | wf (Split (f,g,offset',overlap),offset,dropTail) =
        let
          val fTask = fn () => wf (f,offset,overlap)
          val gTask = fn () => wf (g,offset + offset',dropTail)
        in
          ignore (Primitives.par (fTask,gTask))
        end
  in
    fn frag => wf (frag,0,false)
  end

  fun seqFromFrag frag =
  let
    val n = fragLength frag
    val dest = Seq.tabulate (fn _ => ref NONE) n
    val () = writeFrag dest frag
  in
    Seq.map (Option.valOf o !) dest
  end

  datatype ownedFrag = S1 of frag | S2 of frag
  fun disown (S1 f) = f
    | disown (S2 f) = f
  (* perfect split *)
  fun fsplit (Empty,g) = g
    | fsplit (f,Empty) = f
    | fsplit (f,g) = Split (f,g,fragLength f,false)
  (* one overlap *)
  fun fsplit1 (Empty,g) = g
    | fsplit1 (f,Empty) = f
    | fsplit1 (f,g) = Split (f,g,fragLength f - 1,true)
  fun lExpectS1 (S2 l,m,r') = (S1 Empty,fsplit (l,m),r')
    | lExpectS1 rest = rest
  fun lExpectS2 (S1 l,m,r') = (S2 Empty,fsplit (l,m),r')
    | lExpectS2 rest = rest
  fun rExpectS1 (l',m,S2 r) = (l',fsplit (m,r),S1 Empty)
    | rExpectS1 rest = rest
  fun rExpectS2 (l',m,S1 r) = (l',fsplit (m,r),S2 Empty)
    | rExpectS2 rest = rest

  fun mergeHelper (s1 : t seq, s2 : t seq) : ownedFrag * frag * ownedFrag =
  (* (fn (l',m,r') => ( *)
  (*   let val sts = sts o seqFromFrag val sts' = sts o disown *)
  (*   in print (sts' l' ^ " " ^ sts m ^ " " ^ sts' r' ^ "\n") end; *)
  (*   (l',m,r') *)
  (* )) *)
  let
    (* val () = print ("merge " ^ sts s1 ^ " " ^ sts s2 ^ "\n") *)
    val j1 = len s1 div 2
    val j2 = len s2 div 2
  in
    if len s1 = 0 then
      (S1 Empty,Empty,S2 (if len s2 = 0 then Empty else Slice s2))
    else if len s2 = 0 then
      (S1 (Slice s1),Empty,S2 Empty)
    else if lt (last s1,first s2) then
      (S1 (Slice s1),Empty,S2 (Slice s2))
    else if gt (first s1,last s2) then
      (S2 (Slice s2),Empty,S1 (Slice s1))
    else if lt (nth s1 j1,first s2) then
      let val (l',m,r') = lExpectS1 (mergeHelper (drop s1 (j1 + 1),s2))
      in (S1 (fsplit (Slice (take s1 (j1 + 1)),disown l')),m,r')
      end
    else if gt (nth s1 j1,last s2) then
      let val (l',m,r') = rExpectS1 (mergeHelper (take s1 j1,s2))
      in (l',m,S1 (fsplit (disown r',Slice (drop s1 j1))))
      end
    else if gt (first s1,nth s2 j2) then
      let val (l',m,r') = lExpectS2 (mergeHelper (s1,drop s2 (j2 + 1)))
      in (S2 (fsplit (Slice (take s2 (j2 + 1)),disown l')),m,r')
      end
    else if lt (last s1,nth s2 j2) then
      let val (l',m,r') = rExpectS2 (mergeHelper (s1,take s2 j2))
      in (l',m,S2 (fsplit (disown r',Slice (drop s2 j2))))
      end
    else if lt (nth s1 j1,nth s2 j2) then
      let
        val task1 = fn () => rExpectS2 (mergeHelper (take s1 (j1 + 1),take s2 j2))
        val task2 = fn () => lExpectS2 (rExpectS1 (mergeHelper (drop s1 j1,take s2 (j2 + 1))))
        val task3 = fn () => lExpectS1 (mergeHelper (drop s1 (j1 + 1),drop s2 j2))
        val (((l',m1,_),(_,m2,_)),(_,m3,r')) =
          par (fn () => par (task1,task2),task3)
        val m = fsplit1 (fsplit1 (m1,m2),m3)
      in
        (l',m,r')
      end
    else (* gt (nth s1 j1,nth s2 j2) *)
      let
        val task1 = fn () => rExpectS1 (mergeHelper (take s1 j1,take s2 (j2 + 1)))
        val task2 = fn () => lExpectS1 (rExpectS2 (mergeHelper (take s1 (j1 + 1),drop s2 j2)))
        val task3 = fn () => lExpectS2 (mergeHelper (drop s1 j1,drop s2 (j2 + 1)))
        val (((l',m1,_),(_,m2,_)),(_,m3,r')) =
          par (fn () => par (task1,task2),task3)
        val m = fsplit1 (fsplit1 (m1,m2),m3)
      in
        (l',m,r')
      end
  end

  fun merge (s1 : t seq, s2 : t seq) : t seq =
  let val (l',m,r') = mergeHelper (s1,s2)
  in seqFromFrag (fsplit (fsplit (disown l',m),disown r'))
  end

end
