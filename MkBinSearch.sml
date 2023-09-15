functor MkBinSearch (structure Seq : SEQUENCE structure Ord : ORDERED) :> BINSEARCH
where type t = Ord.t
  and type 'a seq = 'a Seq.t
=
struct

  type t = Ord.t
  type 'a seq = 'a Seq.t

  fun firstGeq' (s, x, l, r) =
    if l >= r then
      l
    else
      let
        val m = l + (r - l) div 2
      in
        case Ord.compare (Seq.nth s m, x) of
            LESS => firstGeq' (s, x, m + 1, r)
          | _ => firstGeq' (s, x, l, m)
      end

  fun firstGt' (s, x, l, r) =
    if l >= r then
      l
    else
      let
        val m = l + (r - l) div 2
      in
        case Ord.compare (Seq.nth s m, x) of
            GREATER => firstGt' (s, x, l, m)
          | _ => firstGt' (s, x, m + 1, r)
      end

  fun firstGeq (s : t seq) (x : t) : int = firstGeq' (s, x, 0, Seq.length s)
  fun firstGt (s : t seq) (x : t) : int = firstGt' (s, x, 0, Seq.length s)
  fun lastLeq (s : t seq) (x : t) : int = firstGt s x - 1
  fun lastLt (s : t seq) (x : t) : int = firstGeq s x - 1

end
