functor MkMergePrl3b (structure Seq : SEQUENCE structure Ord : ORDERED val ts : Ord.t -> string) : MERGE
where type t = Ord.t
  and type 'a seq = 'a Seq.t
=
struct

  (* Satisfy "where" *)
  type t = Ord.t
  type 'a seq = 'a Seq.t

  (* Helpers for "Ord"; for stable sort *)
  infix 4 `< `>
  fun t `< u = Ord.compare (t,u) <> GREATER
  fun t `> u = Ord.compare (t,u) = GREATER

  (* "Seq" helpers for "merge'" *)
  val len = Seq.length
  fun first s = Seq.nth s 0
  fun last s = Seq.nth s (len s - 1)
  fun median s = Seq.nth s (len s div 2)
  fun lHalf s = Seq.take s (len s div 2)
  fun rHalf s = Seq.drop s (len s div 2 + 1)
  fun lHalfI s = Seq.take s (len s div 2 + 1)
  fun rHalfI s = Seq.drop s (len s div 2)

  (* Sugar for "Primitives.par" *)
  infix 1 \\
  fun t \\ u = fn () => Primitives.par (t,u)

  (* Fragment tree type, size hint included *)
  datatype frag =
      F_EMPTY
    | F_SLICE of t seq
    | F_NODE of frag * int * frag * int

  (* Fragment tree size hint; O(1) *)
  fun fraglen F_EMPTY = 0
    | fraglen (F_SLICE s) = len s
    | fraglen (F_NODE (_,m,_,n)) = m + n

  (* Sugar for fragment tree constructors; O(1) *)
  fun toFrag s = if len s = 0 then F_EMPTY else F_SLICE s
  val toFrag1 = toFrag o Seq.singleton
  infixr 5 ++
  fun F_EMPTY ++ g = g
    | f ++ F_EMPTY = f
    | f ++ g       = F_NODE (f,fraglen f,g,fraglen g)

  (* Central logic of merge; work: TBD, span: O(lg (|s| + |t|)) *)
  fun merge' (a,s,b,c,t,d) =
    if b `< c then
      (toFrag s,F_EMPTY,toFrag t)
    else if a `> d then
      (toFrag t,F_EMPTY,toFrag s)
    else if len s = 0 andalso len t = 0 then
      (F_EMPTY,F_EMPTY,F_EMPTY)
    else if len s = 0 then
      if a `< c andalso b `> d then
        (F_EMPTY,toFrag t,F_EMPTY)
      else
        let val p = median t
            val task1 = fn () => merge' (a,s,b,c,lHalf t,p)
            val task2 = fn () => merge' (a,s,b,p,rHalf t,d)
            val ((l1,m1,r1),(l2,m2,r2)) = (task1 \\ task2) ()
        in  if a `> p then
              (l1 ++ toFrag1 p ++ l2,m1 ++ m2,r1 ++ r2)
            else if b `> p then
              (l1 ++ l2,m1 ++ toFrag1 p ++ m2,r1 ++ r2)
            else
              (l1 ++ l2,m1 ++ m2,r1 ++ toFrag1 p ++ r2)
        end
    else if len t = 0 then
      if a `> c andalso b `< d then
        (F_EMPTY,toFrag s,F_EMPTY)
      else
        let val p = median s
            val task1 = fn () => merge' (a,lHalf s,p,c,t,d)
            val task2 = fn () => merge' (p,rHalf s,b,c,t,d)
            val ((l1,m1,r1),(l2,m2,r2)) = (task1 \\ task2) ()
        in  if p `< c then
              (l1 ++ toFrag1 p ++ l2,m1 ++ m2,r1 ++ r2)
            else if p `< d then
              (l1 ++ l2,m1 ++ toFrag1 p ++ m2,r1 ++ r2)
            else
              (l1 ++ l2,m1 ++ m2,r1 ++ toFrag1 p ++ r2)
        end
    else if median s `< c then
      let val (l,m,r) = merge' (median s,rHalf s,b,c,t,d)
      in  (toFrag (lHalfI s) ++ l,m,r)
      end
    else if median s `> d then
      let val (l,m,r) = merge' (a,lHalf s,median s,c,t,d)
      in  (l,m,r ++ toFrag (rHalfI s))
      end
    else if a `> median t then
      let val (l,m,r) = merge' (a,s,b,median t,rHalf t,d)
      in  (toFrag (lHalfI t) ++ l,m,r)
      end
    else if b `< median t then
      let val (l,m,r) = merge' (a,s,b,c,lHalf t,median t)
      in  (l,m,r ++ toFrag (rHalfI t))
      end
    else
      let val (sl,p,sr) = (lHalf s,median s,rHalf s)
          val (tl,q,tr) = (lHalf t,median t,rHalf t)
          val taskL = fn () => merge' (a,sl,p,c,tl,q)
          val taskM1 = fn () => merge' (p,sr,b,c,tl,q)
          val taskM2 = fn () => merge' (a,sl,p,q,tr,d)
          val taskR = fn () => merge' (p,sr,b,q,tr,d)
          val (((l,m1,_),(_,m2,_)),(_,m3,r)) =
            (taskL \\ (if p `< q then taskM1 else taskM2) \\ taskR) ()
      in  if p `< q then
            (l,(m1 ++ toFrag1 p) ++ (m2 ++ toFrag1 q) ++ m3,r)
          else
            (l,(m1 ++ toFrag1 q) ++ (m2 ++ toFrag1 p) ++ m3,r)
      end

  (* Flatten fragment tree into seq; work: O(|f|), span: O(lg |f|) *)
  local
    fun write (_,F_EMPTY) = ()
      | write (buf,F_SLICE s) =
        (ignore o Seq.map (fn (d,x) => d := SOME x) o Seq.zip) (buf,s)
      | write (buf,F_NODE (f,m,g,_)) =
        let val task1 = fn () => write (buf,f)
            val task2 = fn () => write (Seq.drop buf m,g)
        in  ignore ((task1 \\ task2) ())
        end
  in
    fun toSeq f =
    let val buf : t option ref seq = Seq.tabulate (fn _ => ref NONE) (fraglen f)
        val () = write (buf,f)
    in  Seq.map (Option.valOf o !) buf
    end
  end

  fun merge (s : t seq, t : t seq) : t seq =
    if len s = 0 then t
    else if len t = 0 then s
    else
      let val (a,b,c,d) = (first s,last s,first t,last t)
          val (l,m,r) = merge' (a,s,b,c,t,d)
      in  toSeq (l ++ m ++ r)
      end

end
