functor MkMergePrl3a (structure Seq : SEQUENCE structure Ord : ORDERED val ts : Ord.t -> string) : MERGE
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
    | Split of frag * frag * int (* f1 f2 offset *)

  fun fraglen Empty = 0
    | fraglen (Slice s) = len s
    | fraglen (Split (_,g,offset)) = offset + fraglen g

  fun writeSlice dest (s,offset) =
    (ignore o Seq.map (fn (x,d) => d := SOME x) o Seq.zip) (s,drop dest offset)

  fun writeFrag dest =
  let
    val ws = writeSlice dest
    fun wf (Empty,_) = ()
      | wf (Slice s,offset) = ws (s,offset)
      | wf (Split (f,g,offset'),offset) =
        ignore (par (fn () => wf (f,offset),fn () => wf (g,offset + offset')))
  in
    fn frag => wf (frag,0)
  end

  fun seqFromFrag frag =
  let
    val n = fraglen frag
    val dest = Seq.tabulate (fn _ => ref NONE) n
    val () = writeFrag dest frag
  in
    Seq.map (Option.valOf o !) dest
  end

  datatype ownedFrag = S1 of frag | S2 of frag
  val disown = fn S1 f => f | S2 f => f
  fun fsingleton s = if len s = 0 then Empty else Slice s
  fun fsplit (Empty,g) = g
    | fsplit (f,Empty) = f
    | fsplit (f,g) = Split (f,g,fraglen f)
  fun lExpectS1 (S2 l,m,r') = (S1 Empty,fsplit (l,m),r')
    | lExpectS1 rest = rest
  fun lExpectS2 (S1 l,m,r') = (S2 Empty,fsplit (l,m),r')
    | lExpectS2 rest = rest
  fun rExpectS1 (l',m,S2 r) = (l',fsplit (m,r),S1 Empty)
    | rExpectS1 rest = rest
  fun rExpectS2 (l',m,S1 r) = (l',fsplit (m,r),S2 Empty)
    | rExpectS2 rest = rest

  fun mergeHelper (s1 : t seq, vmax1 : t, s2 : t seq, vmax2 : t) : ownedFrag * frag * ownedFrag =
  (* (fn (l',m,r') => ( *)
  (*   let val sts = sts o seqFromFrag val sts' = fn S1 f => "S1" ^ sts f | S2 f => "S2" ^ sts f *)
  (*   in print (sts' l' ^ " " ^ sts m ^ " " ^ sts' r' ^ "\n") end; *)
  (*   (l',m,r') *)
  (* )) *)
  let
    (* val () = print ("merge " ^ sts s1 ^ " " ^ ts vmax1 ^ " " ^ sts s2 ^ " " ^ ts vmax2 ^ "\n") *)
    val vmin1 = if len s1 = 0 then vmax1 else first s1
    val vmin2 = if len s2 = 0 then vmax2 else first s2
    val j1 = len s1 div 2
    val j2 = len s2 div 2
    val vmid1 = if len s1 = 0 then vmax1 else nth s1 j1
    val vmid2 = if len s2 = 0 then vmax2 else nth s2 j2
    val (l1,r1) = (take s1 j1,drop s1 j1)
    val (l2,r2) = (take s2 j2,drop s2 j2)
  in
    if lt (vmax1,vmin2) then
      (S1 (fsingleton s1),Empty,S2 (fsingleton s2))
    else if gt (vmin1,vmax2) then
      (S2 (fsingleton s2),Empty,S1 (fsingleton s1))
    else if len s1 > 0 andalso lt (last s1,vmin2) then
      let val (l',m,r') = lExpectS1 (mergeHelper (Seq.empty(),vmax1,s2,vmax2))
      in (S1 (fsplit (fsingleton s1,disown l')),m,r')
      end
    else if len s2 > 0 andalso gt (vmin1,last s2) then
      let val (l',m,r') = lExpectS2 (mergeHelper (s1,vmax1,Seq.empty(),vmax2))
      in (S2 (fsplit (fsingleton s2,disown r')),m,r')
      end
    (* else if len s1 = 1 andalso len s2 = 0 then *)
    (*   (S1 (Slice s1),Empty,S1 Empty) *)
    (* else if len s1 = 0 andalso len s2 = 1 then *)
    (*   (S2 (Slice s2),Empty,S2 Empty) *)
    (* else if len s1 = 1 andalso len s2 = 1 then *)
    (*   let *)
    (*     val (l',m) = *)
    (*       if lt (vmin1,vmin2) then *)
    (*         (S1 (Slice s1),Slice s2) *)
    (*       else *)
    (*         (S2 (Slice s2),Slice s1) *)
    (*     val r' = if lt (vmax1,vmax2) then S2 Empty else S1 Empty *)
    (*   in *)
    (*     (l',m,r') *)
    (*   end *)
    else if lt (vmid1,vmid2) then
      let
        val task1 = fn () => rExpectS2 (mergeHelper (l1,vmid1,l2,vmid2))
        val task2 = fn () => lExpectS2 (rExpectS1 (mergeHelper (r1,vmax1,l2,vmid2)))
        val task3 = fn () => lExpectS1 (mergeHelper (r1,vmax1,r2,vmax2))
        val (((l',m1,_),(_,m2,_)),(_,m3,r')) =
          par (fn () => par (task1,task2),task3)
        val m = fsplit (fsplit (m1,m2),m3)
      in
        (l',m,r')
      end
    else (* gt (vmid1,vmid2) *)
      let
        val task1 = fn () => rExpectS1 (mergeHelper (l1,vmid1,l2,vmid2))
        val task2 = fn () => lExpectS1 (rExpectS2 (mergeHelper (l1,vmid1,r2,vmax2)))
        val task3 = fn () => lExpectS2 (mergeHelper (r1,vmax1,r2,vmax2))
        val (((l',m1,_),(_,m2,_)),(_,m3,r')) =
          par (fn () => par (task1,task2),task3)
        val m = fsplit (fsplit (m1,m2),m3)
      in
        (l',m,r')
      end
  end

  fun merge (s1 : t seq, s2 : t seq) : t seq =
    if len s1 = 0 then s2
    else if len s2 = 0 then s1
    else
      let val (l',m,r') = mergeHelper (s1,last s1,s2,last s2)
      in seqFromFrag (fsplit (fsplit (disown l',m),disown r'))
      end

end
