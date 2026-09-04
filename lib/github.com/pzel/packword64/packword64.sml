local
  structure V = Word8Vector;
  structure A = Word8Array;

  fun op <<(a,b) : Word64.word =
      Word64.<<(Word8.toLargeWord a, b)

  fun op <!<(a,b) : Word64.word =
      Word64.<<(Word8.toLargeWordX a, b)

  fun op I(a,b) : Word64.word =
      Word64.orb(a, b)

  infix 4 << <!<
  infix 3 I
  val SIZE = 8
in

structure PackWord64Big : PACK_WORD =
struct

val bytesPerElem = SIZE
val isBigEndian = true

fun subVec(a, i) =
    0w0
      I V.sub(a,i*SIZE+0) << 0w56
      I V.sub(a,i*SIZE+1) << 0w48
      I V.sub(a,i*SIZE+2) << 0w40
      I V.sub(a,i*SIZE+3) << 0w32
      I V.sub(a,i*SIZE+4) << 0w24
      I V.sub(a,i*SIZE+5) << 0w16
      I V.sub(a,i*SIZE+6) << 0w8
      I V.sub(a,i*SIZE+7) << 0w0

fun subVecX(a, i) =
    0w0
      I V.sub(a,i*SIZE+0) <!< 0w56
      I V.sub(a,i*SIZE+1) << 0w48
      I V.sub(a,i*SIZE+2) << 0w40
      I V.sub(a,i*SIZE+3) << 0w32
      I V.sub(a,i*SIZE+4) << 0w24
      I V.sub(a,i*SIZE+5) << 0w16
      I V.sub(a,i*SIZE+6) << 0w8
      I V.sub(a,i*SIZE+7) << 0w0

fun subArr(a, i) =
    0w0
      I A.sub(a,i*SIZE+0) << 0w56
      I A.sub(a,i*SIZE+1) << 0w48
      I A.sub(a,i*SIZE+2) << 0w40
      I A.sub(a,i*SIZE+3) << 0w32
      I A.sub(a,i*SIZE+4) << 0w24
      I A.sub(a,i*SIZE+5) << 0w16
      I A.sub(a,i*SIZE+6) << 0w8
      I A.sub(a,i*SIZE+7) << 0w0

fun subArrX(a, i) =
    0w0
      I A.sub(a,i*SIZE+0) <!< 0w56
      I A.sub(a,i*SIZE+1) << 0w48
      I A.sub(a,i*SIZE+2) << 0w40
      I A.sub(a,i*SIZE+3) << 0w32
      I A.sub(a,i*SIZE+4) << 0w24
      I A.sub(a,i*SIZE+5) << 0w16
      I A.sub(a,i*SIZE+6) << 0w8
      I A.sub(a,i*SIZE+7) << 0w0

fun update(a: A.array, i : int, v: LargeWord.word) =
    if i < 0 orelse i*SIZE+7 >= Word8Array.length a
    then raise Subscript
    else let infix >>
             val op >> = LargeWord.>>
         in
           (Word8Array.update(a, i*SIZE+7, Word8.fromLargeWord((v >> 0w0)));
            Word8Array.update(a, i*SIZE+6, Word8.fromLargeWord(v >> 0w8));
            Word8Array.update(a, i*SIZE+5, Word8.fromLargeWord(v >> 0w16));
            Word8Array.update(a, i*SIZE+4, Word8.fromLargeWord(v >> 0w24));
            Word8Array.update(a, i*SIZE+3, Word8.fromLargeWord(v >> 0w32));
            Word8Array.update(a, i*SIZE+2, Word8.fromLargeWord(v >> 0w40));
            Word8Array.update(a, i*SIZE+1, Word8.fromLargeWord(v >> 0w48));
            Word8Array.update(a, i*SIZE+0, Word8.fromLargeWord(v >> 0w56)))
         end;
end

structure PackWord64Little : PACK_WORD =
struct
val bytesPerElem = SIZE
val isBigEndian = false

fun x () = raise Fail "TODO"

fun subVec(a, i) =
    0w0
      I V.sub(a,i*SIZE+7) << 0w56
      I V.sub(a,i*SIZE+6) << 0w48
      I V.sub(a,i*SIZE+5) << 0w40
      I V.sub(a,i*SIZE+4) << 0w32
      I V.sub(a,i*SIZE+3) << 0w24
      I V.sub(a,i*SIZE+2) << 0w16
      I V.sub(a,i*SIZE+1) << 0w8
      I V.sub(a,i*SIZE+0) << 0w0

fun subVecX(a, i) =
    0w0
      I V.sub(a,i*SIZE+7) <!< 0w56
      I V.sub(a,i*SIZE+6) << 0w48
      I V.sub(a,i*SIZE+5) << 0w40
      I V.sub(a,i*SIZE+4) << 0w32
      I V.sub(a,i*SIZE+3) << 0w24
      I V.sub(a,i*SIZE+2) << 0w16
      I V.sub(a,i*SIZE+1) << 0w8
      I V.sub(a,i*SIZE+0) << 0w0


fun subArr(a, i) =
    0w0
      I A.sub(a,i*SIZE+7) << 0w56
      I A.sub(a,i*SIZE+6) << 0w48
      I A.sub(a,i*SIZE+5) << 0w40
      I A.sub(a,i*SIZE+4) << 0w32
      I A.sub(a,i*SIZE+3) << 0w24
      I A.sub(a,i*SIZE+2) << 0w16
      I A.sub(a,i*SIZE+1) << 0w8
      I A.sub(a,i*SIZE+0) << 0w0

fun subArrX(a, i) =
    0w0
      I A.sub(a,i*SIZE+7) <!< 0w56
      I A.sub(a,i*SIZE+6) << 0w48
      I A.sub(a,i*SIZE+5) << 0w40
      I A.sub(a,i*SIZE+4) << 0w32
      I A.sub(a,i*SIZE+3) << 0w24
      I A.sub(a,i*SIZE+2) << 0w16
      I A.sub(a,i*SIZE+1) << 0w8
      I A.sub(a,i*SIZE+0) << 0w0

fun update(a: A.array, i : int, v: LargeWord.word) =
    if i < 0 orelse i*SIZE+7 >= Word8Array.length a
    then raise Subscript
    else let infix >>
             val op >> = LargeWord.>>
         in
           (Word8Array.update(a, i*SIZE+0, Word8.fromLargeWord((v >> 0w0)));
            Word8Array.update(a, i*SIZE+1, Word8.fromLargeWord(v >> 0w8));
            Word8Array.update(a, i*SIZE+2, Word8.fromLargeWord(v >> 0w16));
            Word8Array.update(a, i*SIZE+3, Word8.fromLargeWord(v >> 0w24));
            Word8Array.update(a, i*SIZE+4, Word8.fromLargeWord(v >> 0w32));
            Word8Array.update(a, i*SIZE+5, Word8.fromLargeWord(v >> 0w40));
            Word8Array.update(a, i*SIZE+6, Word8.fromLargeWord(v >> 0w48));
            Word8Array.update(a, i*SIZE+7, Word8.fromLargeWord(v >> 0w56)))
         end;
end (* struct *)
end (* local *)
