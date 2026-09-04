structure V = Word8Vector
structure A = Word8Array

type raw = Word8Vector.elem list

val inputWBig : LargeWord.word = 0w72623864001003004
val inputWLittle : LargeWord.word = 0w18230007237903057409

val input1 : raw =
    [0w1, 0w2, 0w3, 0w4, 0w255, 0w254, 0w253, 0w252];

val smallestSigned =
    Word64.fromLargeInt ~9223372036854775808

val largestSigned =
    Word64.fromLargeInt 9223372036854775807

val smallestSignedBig : raw =
    [0w128,0w0,0w0,0w0,0w0,0w0,0w0,0w0]

val smallestSignedLittle : raw =
    [0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w128]

val largestSignedBig : raw =
    [0w127,0w255,0w255,0w255,0w255,0w255,0w255,0w255]

val largestSignedLittle : raw =
    [0w255,0w255,0w255,0w255,0w255,0w255,0w255,0w127]

val bigEndianTests = [
  It "has eight bytes per elem"
     (fn ()=> let val op == = Assert.eq Int.toString
              in PackWord64Big.bytesPerElem == 8
              end),
  It "is big endian"
     (fn ()=> let val op == = Assert.eq Bool.toString
              in PackWord64Big.isBigEndian == true
              end),
  It "can convert a big-endian vector"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = V.fromList input1
              in PackWord64Big.subVec(input, 0) == 0w72623864001003004
              end),
  It "can convert a big-endian vector at offset=1"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = V.fromList (smallestSignedBig @ input1)
              in PackWord64Big.subVec(input, 1) == 0w72623864001003004
              end),
  It "can convert a big-endian vector at offset=2"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = V.fromList (input1 @ input1 @ smallestSignedBig @ input1)
              in PackWord64Big.subVec(input, 2) == smallestSigned
              end),

  It "will raise Subscript if the offset is too large"
     (fn () => Subscript != (fn ()=> PackWord64Big.subVec(V.fromList input1, 1))),

  It "can convert the biggest signed int64"
     (fn () => let val op == = Assert.eq Word64.toString
                  val input = V.fromList largestSignedBig
              in PackWord64Big.subVec(input, 0) == largestSigned
              end),
  It "can convert the smallest signed int64"
     (fn () => let val op == = Assert.eq Word64.toString
                  val input = V.fromList smallestSignedBig
              in PackWord64Big.subVec(input, 0) == smallestSigned
              end),
  It "can convert a big-endian array"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = A.fromList input1
              in PackWord64Big.subArr(input, 0) == 0w72623864001003004
              end),
  It "can convertX a big-endian array"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = A.fromList input1
              in PackWord64Big.subArrX(input, 0) == 0w72623864001003004
              end),
  It "can convert a big-endian array at offset"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = A.fromList (input1 @ largestSignedBig)
              in PackWord64Big.subArr(input, 1) == largestSigned
              end),

  It "can update an array with a word64"
     (fn ()=> let val op =/= = Assert.neq (fn x => x)
                  val buffer = A.array(8, 0w0)
                  val _ = PackWord64Big.update(buffer, 0, inputWBig)
                  val res = Byte.unpackString (Word8ArraySlice.full (buffer))
              in
                res =/= "\000\000\000\000\000\000\000\000"
              end),

  It "can update an array with a word64 and read it back"
     (fn ()=> let val op =/= = Assert.neq (fn x => x)
                  val op == = Assert.eq Word64.toString
                  val buffer = A.array(8, 0w0)
                  val _ = PackWord64Big.update(buffer, 0, inputWBig)
                  val res = PackWord64Big.subArr(buffer, 0)
              in
                res == inputWBig
              end),

  It "can update an array at index with a word64 and read it back"
     (fn ()=> let val op =/= = Assert.neq (fn x => x)
                  val op == = Assert.eq Word64.toString
                  val buffer = A.array(8*4, 0w0)
                  val _ = PackWord64Big.update(buffer, 3, inputWBig)
                  val res = PackWord64Big.subArr(buffer, 3)
              in
                res == inputWBig
              end)
]

val littleEndianTests = [
  It "has eight bytes per elem"
     (fn ()=> let val op == = Assert.eq Int.toString
              in PackWord64Little.bytesPerElem == 8
              end),
  It "is not big endian"
     (fn ()=> let val op == = Assert.eq Bool.toString
              in PackWord64Little.isBigEndian == false
              end),
  It "can convert a little-endian vector"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = V.fromList input1
              in PackWord64Little.subVec(input, 0) == 0w18230007237903057409
              end),
  It "can convert the biggest signed int64"
     (fn () => let val op == = Assert.eq Word64.toString
                  val input = V.fromList largestSignedLittle
              in PackWord64Little.subVec(input, 0) == largestSigned
              end),
  It "can convert the smallest signed int64"
     (fn () => let val op == = Assert.eq Word64.toString
                  val input = V.fromList smallestSignedLittle
              in PackWord64Little.subVec(input, 0) == smallestSigned
              end),
  It "can convertX a little-endian vector"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = V.fromList input1
              in PackWord64Little.subVecX(input, 0) == 0w18230007237903057409
              end),
  It "can convert a little-endian array"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = A.fromList input1
              in PackWord64Little.subArr(input, 0) == 0w18230007237903057409
              end),
  It "can convertX a little-endian array"
     (fn ()=> let val op == = Assert.eq Word64.toString
                  val input = A.fromList input1
              in PackWord64Little.subArrX(input, 0) == 0w18230007237903057409
              end),
  It "can update an array with a word64"
     (fn ()=> let val op =/= = Assert.neq (fn x => x)
                  val buffer = A.array(8, 0w0)
                  val _ = PackWord64Little.update(buffer, 0, inputWLittle)
                  val res = Byte.unpackString (Word8ArraySlice.full (buffer))
              in
                res =/= "\000\000\000\000\000\000\000\000"
              end),

  It "can update an array with a word64 and read it back"
     (fn ()=> let val op =/= = Assert.neq (fn x => x)
                  val op == = Assert.eq Word64.toString
                  val buffer = A.array(8, 0w0)
                  val _ = PackWord64Little.update(buffer, 0, inputWLittle)
                  val res = PackWord64Little.subArr(buffer, 0)
              in
                res == inputWLittle
              end),

  It "can update an array at index with a word64 and read it back"
     (fn ()=> let val op =/= = Assert.neq (fn x => x)
                  val op == = Assert.eq Word64.toString
                  val buffer = A.array(8*4, 0w0)
                  val _ = PackWord64Little.update(buffer, 3, inputWLittle)
                  val res = PackWord64Little.subArr(buffer, 3)
              in
                res == inputWLittle
              end)

]


fun main () =
	runTestsWith (bigEndianTests @ littleEndianTests)
               (CommandLine.arguments())
