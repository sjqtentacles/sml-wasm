(* leb128.sml — standard LEB128 codec (as used by WebAssembly / DWARF).

   Little-endian, seven payload bits per byte; the 0x80 bit signals that more
   bytes follow.  Signed values use two's-complement with the final byte's
   0x40 bit acting as the sign bit. *)
structure Leb128 :> LEB128 =
struct
  exception Leb128 of string

  fun toIntList v =
    Word8Vector.foldr (fn (b, acc) => Word8.toInt b :: acc) [] v

  (* ---- encode ---- *)

  fun encodeU n =
    if n < 0 then raise Leb128 "encodeU: negative"
    else
      let
        fun loop (v, acc) =
          let
            val byte = IntInf.toInt (IntInf.andb (v, 0x7F))
            val v'   = IntInf.~>> (v, 0w7)            (* v div 128, v >= 0 *)
          in
            if v' = 0 then List.rev (Word8.fromInt byte :: acc)
            else loop (v', Word8.fromInt (byte + 0x80) :: acc)
          end
      in
        Word8Vector.fromList (loop (n, []))
      end

  fun encodeS n =
    let
      fun loop (v, acc) =
        let
          val byte    = IntInf.toInt (IntInf.andb (v, 0x7F))
          val v'      = IntInf.~>> (v, 0w7)           (* arithmetic shift *)
          val signSet = byte >= 0x40                  (* payload bit 6 set *)
          val done    = (v' = 0  andalso not signSet)
                        orelse (v' = ~1 andalso signSet)
        in
          if done then List.rev (Word8.fromInt byte :: acc)
          else loop (v', Word8.fromInt (byte + 0x80) :: acc)
        end
    in
      Word8Vector.fromList (loop (n, []))
    end

  fun encodeUList n = toIntList (encodeU n)
  fun encodeSList n = toIntList (encodeS n)

  (* ---- decode ---- *)

  fun decodeU (vec, start) =
    let
      val n = Word8Vector.length vec
      fun loop (i, shift, acc) =
        if i >= n then raise Leb128 "decodeU: truncated"
        else
          let
            val raw  = Word8Vector.sub (vec, i)
            val low  = Word8.toInt (Word8.andb (raw, 0wx7F))
            val acc' = acc + IntInf.<< (IntInf.fromInt low, shift)
            val more = Word8.andb (raw, 0wx80) <> 0w0
          in
            if more then loop (i + 1, shift + 0w7, acc')
            else (acc', i + 1)
          end
    in
      loop (start, 0w0, 0)
    end

  fun decodeS (vec, start) =
    let
      val n = Word8Vector.length vec
      fun loop (i, shift, acc) =
        if i >= n then raise Leb128 "decodeS: truncated"
        else
          let
            val raw    = Word8Vector.sub (vec, i)
            val low    = Word8.toInt (Word8.andb (raw, 0wx7F))
            val acc'   = acc + IntInf.<< (IntInf.fromInt low, shift)
            val shift' = shift + 0w7
            val more   = Word8.andb (raw, 0wx80) <> 0w0
          in
            if more then loop (i + 1, shift', acc')
            else
              let
                val signSet = Word8.andb (raw, 0wx40) <> 0w0
                val result  = if signSet
                              then acc' - IntInf.<< (1, shift')   (* sign extend *)
                              else acc'
              in (result, i + 1) end
          end
    in
      loop (start, 0w0, 0)
    end
end
