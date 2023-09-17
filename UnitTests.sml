structure UnitTests =
struct

  structure Seq : SEQUENCE = ArraySequence
  structure Ord : ORDERED =
  struct
    type t = int
    val compare = Int.compare
  end

  structure Mp : MERGE = MkMergePrl (structure Seq = Seq structure Ord = Ord val ts = Int.toString)
  structure Mp2 : MERGE = MkMergePrl2 (structure Seq = Seq structure Ord = Ord val ts = Int.toString)
  structure Mp3 : MERGE = MkMergePrl3 (structure Seq = Seq structure Ord = Ord val ts = Int.toString)
  structure Mp3a : MERGE = MkMergePrl3a (structure Seq = Seq structure Ord = Ord val ts = Int.toString)
  structure Mp3b : MERGE = MkMergePrl3b (structure Seq = Seq structure Ord = Ord val ts = Int.toString)

  fun seqEqual elEqual (s1, s2) =
    Seq.length s1 = Seq.length s2
    andalso Seq.reduce (fn (a, b) => a andalso b) true (Seq.map elEqual (Seq.zip (s1, s2)))

  fun test (elEqual, elToString, merge) ((lInput1, lInput2), lAnswer) =
  let
    val s1 = Seq.fromList lInput1
    val s2 = Seq.fromList lInput2
    val sA = Seq.fromList lAnswer
    val se = seqEqual elEqual
    val ts = Seq.toString elToString
  in
    let
      val sR = merge (s1, s2)
    in
      if se (sR, sA) then
        (print "Test passed.\n"; true)
      else
        (
          print (
            "Test failed on input (" ^ ts s1 ^ "," ^ ts s2 ^ "): expected "
            ^ ts sA ^ ", got " ^ ts sR ^ ".\n"
          );
          false
        )
    end
    handle
      e =>
      (
        print (
          "Test failed on input (" ^ ts s1 ^ "," ^ ts s2 ^ "): exn "
          ^ exnMessage e ^ ".\n"
        );
        false
      )
  end

  fun testAll params tests =
  let
    val t' = test params
    fun t (weight, ins, outs) = if t' (ins, outs) then weight else 0
    val testSeq = Seq.fromList tests
  in
    (Seq.reduce op+ 0 (Seq.map t testSeq), Seq.reduce op+ 0 (Seq.map #1 testSeq))
  end

  val tests =
    [
      (1, ([], []), []),
      (1, ([0], [1]), [0,1]),
      (1, ([1], [0]), [0,1]),
      (1, ([1,3,7,9], [2,4,6,8]), [1,2,3,4,6,7,8,9]),
      (1, ([1,3,4,5], [2,3,4,4]), [1,2,3,3,4,4,4,5]),
      (1, ([1,2,5,6,8,9], []), [1,2,5,6,8,9]),
      (1,([0,4,5,7,8,10,12,15,17,19],[1,2,3,6,9,11,13,14,16,18]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([0,1,3,4,6,8,10,16,17,18],[2,5,7,9,11,12,13,14,15,19]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([1,3,4,5,8,10,11,12,15,17],[0,2,6,7,9,13,14,16,18,19]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([2,3,4,5,6,8,9,12,14,16],[0,1,7,10,11,13,15,17,18,19]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([0,3,6,9,10,11,12,13,15,17],[1,2,4,5,7,8,14,16,18,19]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([0,3,4,5,10,13,14,15,17,19],[1,2,6,7,8,9,11,12,16,18]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([0,2,4,5,6,7,10,11,16,17],[1,3,8,9,12,13,14,15,18,19]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([0,2,4,8,9,11,12,13,15,19],[1,3,5,6,7,10,14,16,17,18]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([0,3,6,7,8,9,11,12,16,17],[1,2,4,5,10,13,14,15,18,19]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([0,3,5,6,7,8,13,15,16,17],[1,2,4,9,10,11,12,14,18,19]),[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]),
      (1,([1,1,1,1,1,1,2,3,3,4,4,4,5,5,6],[0,1,1,1,4,4,4,4,5,5,6,6,7,7,9]),[0,1,1,1,1,1,1,1,1,1,2,3,3,4,4,4,4,4,4,4,5,5,5,5,6,6,6,7,7,9]),
      (1,([0,1,2,2,2,2,2,3,3,4,6,6,7,8,9],[0,1,2,2,3,3,5,6,6,7,7,8,8,8,9]),[0,0,1,1,2,2,2,2,2,2,2,3,3,3,3,4,5,6,6,6,6,7,7,7,8,8,8,8,9,9]),
      (1,([0,0,0,1,1,2,2,3,3,3,4,5,7,7,9],[0,1,1,1,2,3,3,4,5,5,7,7,9,9,9]),[0,0,0,0,1,1,1,1,1,2,2,2,3,3,3,3,3,4,4,5,5,5,7,7,7,7,9,9,9,9]),
      (1,([0,0,1,1,4,4,4,5,5,7,8,8,8,9,9],[2,2,2,3,4,5,5,5,6,6,8,8,9,9,9]),[0,0,1,1,2,2,2,3,4,4,4,4,5,5,5,5,5,6,6,7,8,8,8,8,8,9,9,9,9,9]),
      (1,([0,0,0,1,1,1,1,2,2,5,5,5,7,7,9],[0,0,0,1,3,3,3,3,6,6,7,8,8,9,9]),[0,0,0,0,0,0,1,1,1,1,1,2,2,3,3,3,3,5,5,5,6,6,7,7,7,8,8,9,9,9]),
      (1,([0,0,2,2,2,3,4,4,5,6,6,6,7,8,9],[0,1,1,1,2,3,3,3,5,7,8,8,8,8,9]),[0,0,0,1,1,1,2,2,2,2,3,3,3,3,4,4,5,5,6,6,6,7,7,8,8,8,8,8,9,9]),
      (1,([1,1,2,3,3,3,4,4,4,5,6,6,7,9,9],[3,3,3,3,3,4,5,6,6,8,8,8,9,9,9]),[1,1,2,3,3,3,3,3,3,3,3,4,4,4,4,5,5,6,6,6,6,7,8,8,8,9,9,9,9,9]),
      (1,([0,0,1,1,2,2,4,5,5,6,6,6,7,7,9],[0,0,0,1,1,1,3,4,4,5,6,7,7,7,8]),[0,0,0,0,0,1,1,1,1,1,2,2,3,4,4,4,5,5,5,6,6,6,6,7,7,7,7,7,8,9]),
      (1,([0,0,1,1,1,2,3,3,4,4,7,8,9,9,9],[1,1,3,4,5,5,6,6,6,7,7,7,9,9,9]),[0,0,1,1,1,1,1,2,3,3,3,4,4,4,5,5,6,6,6,7,7,7,7,8,9,9,9,9,9,9]),
      (1,([1,3,3,4,4,4,5,5,6,6,6,7,7,7,9],[0,1,2,3,4,4,5,5,6,7,7,7,8,8,8]),[0,1,1,2,3,3,3,4,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,7,7,8,8,8,9])
    ]

  fun run () =
    (
      print "Testing MergePrl\n";
      testAll (op=, Int.toString, Mp.merge) tests;
      print "Testing MergePrl2\n";
      testAll (op=, Int.toString, Mp2.merge) tests;
      print "Testing MergePrl3\n";
      testAll (op=, Int.toString, Mp3.merge) tests;
      print "Testing MergePrl3a\n";
      testAll (op=, Int.toString, Mp3a.merge) tests;
      print "Testing MergePrl3b\n";
      testAll (op=, Int.toString, Mp3b.merge) tests;
      ()
    )
end

