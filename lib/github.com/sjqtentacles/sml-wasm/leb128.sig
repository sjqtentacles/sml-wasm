(* leb128.sig — LEB128 variable-length integer codec.

   This is the *standard* LEB128 used by WebAssembly (and DWARF), which is a
   different encoding from SQLite's big-endian varint.  Numbers are encoded
   little-endian, seven bits per byte, the high bit (0x80) marking
   continuation.  Unsigned values use [encodeU]/[decodeU]; signed values use
   [encodeS]/[decodeS] (two's-complement with sign extension). *)
signature LEB128 =
sig
  (* Raised on a truncated byte stream or an over-long encoding. *)
  exception Leb128 of string

  (* Encode a non-negative integer (unsigned LEB128). *)
  val encodeU : IntInf.int -> Word8Vector.vector
  (* Encode any integer (signed LEB128). *)
  val encodeS : IntInf.int -> Word8Vector.vector

  (* [decodeU (bytes, i)] decodes an unsigned value starting at index [i],
     returning the value and the index one past the last byte consumed. *)
  val decodeU : Word8Vector.vector * int -> IntInf.int * int
  (* As [decodeU] but for signed LEB128. *)
  val decodeS : Word8Vector.vector * int -> IntInf.int * int

  (* Convenience wrappers operating on byte lists (handy for tests). *)
  val encodeUList : IntInf.int -> int list
  val encodeSList : IntInf.int -> int list
end
