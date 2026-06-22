(* decode.sml — WebAssembly binary-format module decoder.

   Reads the `\0asm` magic + version-1 header, then walks the section
   sequence.  Each section is `id : u8`, `size : u32(LEB)`, then `size` bytes
   of payload; unknown / custom / data sections are skipped by size.  The
   function section (type indices) and the code section (locals + bodies) are
   stitched together into the [WasmAst.func] list. *)
structure Decode :> DECODE =
struct
  open WasmAst
  exception Decode of string

  fun toI32 (n : IntInf.int) : Int32.int =
    let
      val m = IntInf.mod (n, 0x100000000)
      val s = if m >= 0x80000000 then m - 0x100000000 else m
    in Int32.fromLarge s end

  fun decode vec =
    let
      val len = Word8Vector.length vec
      val pos = ref 0

      fun atEnd () = !pos >= len

      fun u8 () =
        if !pos >= len then raise Decode "unexpected end of input"
        else let val b = Word8Vector.sub (vec, !pos)
             in pos := !pos + 1; Word8.toInt b end

      fun peek8 () =
        if !pos >= len then raise Decode "unexpected end of input"
        else Word8.toInt (Word8Vector.sub (vec, !pos))

      fun uLEB () =
        let val (v, next) = Leb128.decodeU (vec, !pos)
                            handle _ => raise Decode "malformed LEB128"
        in pos := next; v end

      fun sLEB () =
        let val (v, next) = Leb128.decodeS (vec, !pos)
                            handle _ => raise Decode "malformed LEB128"
        in pos := next; v end

      fun uInt () = IntInf.toInt (uLEB ())

      fun readVec f =
        let
          val n = uInt ()
          fun loop (0, acc) = List.rev acc
            | loop (k, acc) = loop (k - 1, f () :: acc)
        in loop (n, []) end

      fun valtypeOf 0x7F = I32T
        | valtypeOf 0x7E = I64T
        | valtypeOf 0x7D = raise Decode "f32 value type unsupported"
        | valtypeOf 0x7C = raise Decode "f64 value type unsupported"
        | valtypeOf b    =
            raise Decode ("bad value type 0x" ^ Int.fmt StringCvt.HEX b)

      fun readValtype () = valtypeOf (u8 ())

      fun readName () =
        let
          val n  = uInt ()
          val cs = List.tabulate (n, fn _ => Char.chr (u8 ()))
        in String.implode cs end

      fun readBlockType () =
        let val b = peek8 ()
        in
          if b = 0x40 then (ignore (u8 ()); BTEmpty)
          else if b = 0x7F orelse b = 0x7E orelse b = 0x7D orelse b = 0x7C
            then BTVal (readValtype ())
          else BTType (IntInf.toInt (sLEB ()))
        end

      (* parse instructions up to (and consuming) an `end` (0x0B) or `else`
         (0x05); returns the body and the terminator opcode. *)
      fun parseInstrs () =
        let
          fun loop acc =
            let val opc = u8 ()
            in
              case opc of
                0x0B => (List.rev acc, 0x0B)
              | 0x05 => (List.rev acc, 0x05)
              | _    => loop (parseOne opc :: acc)
            end
        in loop [] end

      and parseOne opc =
        case opc of
          0x00 => Unreachable
        | 0x01 => Nop
        | 0x02 => let val bt = readBlockType ()
                      val (body, _) = parseInstrs ()
                  in Block (bt, body) end
        | 0x03 => let val bt = readBlockType ()
                      val (body, _) = parseInstrs ()
                  in Loop (bt, body) end
        | 0x04 => let
                    val bt = readBlockType ()
                    val (thn, term) = parseInstrs ()
                    val els = if term = 0x05 then #1 (parseInstrs ()) else []
                  in If (bt, thn, els) end
        | 0x0C => Br   (uInt ())
        | 0x0D => BrIf (uInt ())
        | 0x0F => Return
        | 0x10 => Call (uInt ())
        | 0x1A => Drop
        | 0x1B => Select
        | 0x20 => LocalGet  (uInt ())
        | 0x21 => LocalSet  (uInt ())
        | 0x22 => LocalTee  (uInt ())
        | 0x23 => GlobalGet (uInt ())
        | 0x24 => GlobalSet (uInt ())
        | 0x41 => I32Const (toI32 (sLEB ()))
        | 0x42 => I64Const (sLEB ())
        (* i32 comparisons *)
        | 0x45 => I32Eqz | 0x46 => I32Eq  | 0x47 => I32Ne
        | 0x48 => I32LtS | 0x49 => I32LtU | 0x4A => I32GtS | 0x4B => I32GtU
        | 0x4C => I32LeS | 0x4D => I32LeU | 0x4E => I32GeS | 0x4F => I32GeU
        (* i64 comparisons *)
        | 0x50 => I64Eqz | 0x51 => I64Eq  | 0x52 => I64Ne
        | 0x53 => I64LtS | 0x54 => I64LtU | 0x55 => I64GtS | 0x56 => I64GtU
        | 0x57 => I64LeS | 0x58 => I64LeU | 0x59 => I64GeS | 0x5A => I64GeU
        (* i32 arithmetic / bitwise *)
        | 0x6A => I32Add | 0x6B => I32Sub  | 0x6C => I32Mul
        | 0x6D => I32DivS| 0x6E => I32DivU | 0x6F => I32RemS | 0x70 => I32RemU
        | 0x71 => I32And | 0x72 => I32Or   | 0x73 => I32Xor
        | 0x74 => I32Shl | 0x75 => I32ShrS | 0x76 => I32ShrU
        | 0x77 => I32Rotl| 0x78 => I32Rotr
        (* i64 arithmetic / bitwise *)
        | 0x7C => I64Add | 0x7D => I64Sub  | 0x7E => I64Mul
        | 0x7F => I64DivS| 0x80 => I64DivU | 0x81 => I64RemS | 0x82 => I64RemU
        | 0x83 => I64And | 0x84 => I64Or   | 0x85 => I64Xor
        | 0x86 => I64Shl | 0x87 => I64ShrS | 0x88 => I64ShrU
        | 0x89 => I64Rotl| 0x8A => I64Rotr
        | _ => raise Decode ("unsupported opcode 0x" ^ Int.fmt StringCvt.HEX opc)

      fun readFunctype () =
        let
          val form = u8 ()
          val () = if form = 0x60 then ()
                   else raise Decode "bad function type form"
          val ps = readVec readValtype
          val rs = readVec readValtype
        in { params = ps, results = rs } end

      fun readLimits () =
        let val flag = u8 ()
        in case flag of
             0x00 => ignore (uInt ())
           | 0x01 => (ignore (uInt ()); ignore (uInt ()))
           | _ => raise Decode "bad limits flag"
        end

      fun readTabletype () = (ignore (u8 ()); readLimits ())  (* elemtype + limits *)

      (* returns true iff the import is a function import *)
      fun readImport () =
        let
          val _ = readName ()   (* module name  *)
          val _ = readName ()   (* field name   *)
          val kind = u8 ()
        in
          case kind of
            0x00 => (ignore (uInt ()); true)            (* func: typeidx *)
          | 0x01 => (readTabletype (); false)
          | 0x02 => (readLimits (); false)
          | 0x03 => (ignore (readValtype ()); ignore (u8 ()); false) (* global *)
          | _ => raise Decode "bad import kind"
        end

      fun readGlobal () =
        let
          val typ = readValtype ()
          val m   = u8 ()
          val mut = case m of 0x00 => Const | 0x01 => Var
                            | _ => raise Decode "bad mutability"
          val (init, _) = parseInstrs ()
        in { typ = typ, mut = mut, init = init } end

      fun readExport () =
        let
          val name = readName ()
          val k    = u8 ()
          val idx  = uInt ()
          val kind = case k of 0x00 => FuncExport | 0x01 => TableExport
                             | 0x02 => MemExport  | 0x03 => GlobalExport
                             | _ => raise Decode "bad export kind"
        in { name = name, kind = kind, index = idx } end

      fun readLocals () =
        let
          val ndecls = uInt ()
          fun loop (0, acc) = List.concat (List.rev acc)
            | loop (k, acc) =
                let val cnt = uInt ()
                    val vt  = readValtype ()
                in loop (k - 1, List.tabulate (cnt, fn _ => vt) :: acc) end
        in loop (ndecls, []) end

      fun readCode () =
        let
          val size    = uInt ()
          val codeEnd = !pos + size
          val locals  = readLocals ()
          val (body, _) = parseInstrs ()
          val () = pos := codeEnd
        in { locals = locals, body = body } end

      (* accumulators *)
      val types       = ref ([] : functype list)
      val funcTypeIdx = ref ([] : int list)
      val codeEntries = ref ([] : { locals : valtype list, body : instr list } list)
      val globals     = ref ([] : global list)
      val exports     = ref ([] : export list)
      val start       = ref (NONE : int option)
      val numImported = ref 0

      (* ---- header ---- *)
      val () = if len < 8 then raise Decode "truncated header" else ()
      val magic = [u8 (), u8 (), u8 (), u8 ()]
      val () = if magic = [0x00, 0x61, 0x73, 0x6D] then ()
               else raise Decode "bad magic (not a wasm module)"
      val ver = [u8 (), u8 (), u8 (), u8 ()]
      val () = if ver = [0x01, 0x00, 0x00, 0x00] then ()
               else raise Decode "unsupported binary version"

      fun sections () =
        if atEnd () then ()
        else
          let
            val id     = u8 ()
            val size   = uInt ()
            val secEnd = !pos + size
            val () =
              (case id of
                 1  => types := readVec readFunctype
               | 2  => numImported := length (List.filter (fn b => b)
                                                  (readVec readImport))
               | 3  => funcTypeIdx := readVec uInt
               | 4  => ignore (readVec readTabletype)
               | 5  => ignore (readVec readLimits)
               | 6  => globals := readVec readGlobal
               | 7  => exports := readVec readExport
               | 8  => start := SOME (uInt ())
               | 10 => codeEntries := readVec readCode
               | _  => ())                            (* custom/elem/data/... *)
            val () = pos := secEnd
          in sections () end

      val () = sections ()

      (* stitch function section + code section into defined functions *)
      val () = if length (!funcTypeIdx) = length (!codeEntries) then ()
               else raise Decode "function/code section length mismatch"
      val funcs =
        ListPair.map
          (fn (ti, code) =>
             { typeIdx = ti, locals = #locals code, body = #body code })
          (!funcTypeIdx, !codeEntries)
    in
      { types            = !types
      , funcs            = funcs
      , globals          = !globals
      , exports          = !exports
      , start            = !start
      , numImportedFuncs = !numImported }
    end
end
