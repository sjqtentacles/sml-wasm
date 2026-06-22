structure Tests =
struct
  open Harness
  open WasmAst

  (* ---------------------------------------------------------------- *)
  (* helpers                                                          *)
  (* ---------------------------------------------------------------- *)

  fun bytes (xs : int list) : Word8Vector.vector =
    Word8Vector.fromList (List.map Word8.fromInt xs)

  fun readBytes path =
    let val s = BinIO.openIn path
        val v = BinIO.inputAll s
    in BinIO.closeIn s; v end

  fun readText path =
    let val s = TextIO.openIn path
        val v = TextIO.inputAll s
    in TextIO.closeIn s; v end

  fun checkLarge name (expected, actual) =
    if expected = actual then check name true
    else (print ("    expected " ^ IntInf.toString expected
                 ^ " but got " ^ IntInf.toString actual ^ "\n");
          check name false)

  fun checkVals name (expected, actual) =
    if expected = actual then check name true
    else (print ("    expected " ^ valuesToString expected
                 ^ " but got " ^ valuesToString actual ^ "\n");
          check name false)

  (* equality-driven instruction-body check with readable failure *)
  fun checkInstrs name (expected : instr list, actual : instr list) =
    checkInt (name ^ " (len)") (length expected, length actual)
    before check name (expected = actual)

  (* ---------------------------------------------------------------- *)
  (* hand-built binary modules (inline byte vectors)                  *)
  (* ---------------------------------------------------------------- *)

  (* add (i32,i32)->i32 — same bytes as the committed add.wasm fixture *)
  val addModuleBytes = bytes
    [ 0x00,0x61,0x73,0x6D, 0x01,0x00,0x00,0x00,
      0x01,0x07,0x01,0x60,0x02,0x7F,0x7F,0x01,0x7F,
      0x03,0x02,0x01,0x00,
      0x07,0x07,0x01,0x03,0x61,0x64,0x64,0x00,0x00,
      0x0A,0x09,0x01,0x07,0x00,0x20,0x00,0x20,0x01,0x6A,0x0B ]

  (* sub (i32,i32)->i32 *)
  val subModuleBytes = bytes
    [ 0x00,0x61,0x73,0x6D, 0x01,0x00,0x00,0x00,
      0x01,0x07,0x01,0x60,0x02,0x7F,0x7F,0x01,0x7F,
      0x03,0x02,0x01,0x00,
      0x07,0x07,0x01,0x03,0x73,0x75,0x62,0x00,0x00,
      0x0A,0x09,0x01,0x07,0x00,0x20,0x00,0x20,0x01,0x6B,0x0B ]

  (* k ()->i64 returning the constant 4294967296 (= 2^32, needs 64 bits).
     i64.const is signed LEB128: 2^32 -> [0x80,0x80,0x80,0x80,0x10]. *)
  val i64ModuleBytes = bytes
    [ 0x00,0x61,0x73,0x6D, 0x01,0x00,0x00,0x00,
      0x01,0x05,0x01,0x60,0x00,0x01,0x7E,
      0x03,0x02,0x01,0x00,
      0x07,0x05,0x01,0x01,0x6B,0x00,0x00,
      0x0A,0x0A,0x01,0x08,0x00,0x42,0x80,0x80,0x80,0x80,0x10,0x0B ]

  (* ---------------------------------------------------------------- *)
  (* inline .wat covering the broader instruction set                 *)
  (* ---------------------------------------------------------------- *)

  val opsWat =
    "(module\n\
    \  (func (export \"sub\")  (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.sub)\n\
    \  (func (export \"divs\") (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.div_s)\n\
    \  (func (export \"rems\") (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.rem_s)\n\
    \  (func (export \"and\")  (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.and)\n\
    \  (func (export \"or\")   (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.or)\n\
    \  (func (export \"xor\")  (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.xor)\n\
    \  (func (export \"shl\")  (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.shl)\n\
    \  (func (export \"shrs\") (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.shr_s)\n\
    \  (func (export \"shru\") (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.shr_u)\n\
    \  (func (export \"lts\")  (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 i32.lt_s)\n\
    \  (func (export \"eqz\")  (param i32) (result i32)\n\
    \     local.get 0 i32.eqz)\n\
    \  (func (export \"sel\")  (param i32) (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 local.get 2 select)\n\
    \  (func (export \"drop2\") (param i32) (param i32) (result i32)\n\
    \     local.get 0 local.get 1 drop)\n\
    \  (func (export \"tee\")  (param i32) (result i32)\n\
    \     i32.const 7 local.tee 0 local.get 0 i32.add)\n\
    \  (func (export \"addwrap32\") (result i32)\n\
    \     i32.const 2147483647 i32.const 1 i32.add)\n\
    \  (func (export \"mulwrap32\") (result i32)\n\
    \     i32.const 65536 i32.const 65536 i32.mul)\n\
    \  (func (export \"addwrap64\") (result i64)\n\
    \     i64.const 9223372036854775807 i64.const 1 i64.add)\n\
    \  (func (export \"div64\") (param i64) (param i64) (result i64)\n\
    \     local.get 0 local.get 1 i64.div_s)\n\
    \  (global $g (mut i32) (i32.const 100))\n\
    \  (func (export \"gget\") (result i32) global.get $g)\n\
    \  (func (export \"gadd\") (param i32) (result i32)\n\
    \     global.get $g local.get 0 i32.add global.set $g global.get $g))\n"

  (* getg ()->i32 reading a mutable global initialized to 42 *)
  val globalModuleBytes = bytes
    [ 0x00,0x61,0x73,0x6D, 0x01,0x00,0x00,0x00,
      0x01,0x05,0x01,0x60,0x00,0x01,0x7F,
      0x03,0x02,0x01,0x00,
      0x06,0x06,0x01,0x7F,0x01,0x41,0x2A,0x0B,
      0x07,0x08,0x01,0x04,0x67,0x65,0x74,0x67,0x00,0x00,
      0x0A,0x06,0x01,0x04,0x00,0x23,0x00,0x0B ]

  (* ---------------------------------------------------------------- *)
  fun runAll () =
    let
      (* ============== LEB128 unsigned reference vectors ============== *)
      val () = section "leb128 unsigned (reference vectors)"
      val () = checkIntList "u 0"       ([0x00],           Leb128.encodeUList 0)
      val () = checkIntList "u 127"     ([0x7F],           Leb128.encodeUList 127)
      val () = checkIntList "u 128"     ([0x80,0x01],      Leb128.encodeUList 128)
      val () = checkIntList "u 300"     ([0xAC,0x02],      Leb128.encodeUList 300)
      val () = checkIntList "u 624485"  ([0xE5,0x8E,0x26], Leb128.encodeUList 624485)

      val () = section "leb128 signed (reference vectors)"
      val () = checkIntList "s 0"    ([0x00],      Leb128.encodeSList 0)
      val () = checkIntList "s -1"   ([0x7F],      Leb128.encodeSList (~1))
      val () = checkIntList "s 63"   ([0x3F],      Leb128.encodeSList 63)
      val () = checkIntList "s -64"  ([0x40],      Leb128.encodeSList (~64))
      val () = checkIntList "s 64"   ([0xC0,0x00], Leb128.encodeSList 64)
      val () = checkIntList "s 128"  ([0x80,0x01], Leb128.encodeSList 128)
      val () = checkIntList "s -128" ([0x80,0x7F], Leb128.encodeSList (~128))
      val () = checkIntList "s 2^32" ([0x80,0x80,0x80,0x80,0x10],
                                      Leb128.encodeSList 4294967296)

      (* ============== LEB128 round trips ============== *)
      val () = section "leb128 round trips"
      fun rtU n = let val (v, _) = Leb128.decodeU (Leb128.encodeU n, 0)
                  in checkLarge ("u rt " ^ IntInf.toString n) (n, v) end
      fun rtS n = let val (v, _) = Leb128.decodeS (Leb128.encodeS n, 0)
                  in checkLarge ("s rt " ^ IntInf.toString n) (n, v) end
      val () = List.app rtU [0,1,127,128,255,300,16384,624485,4294967296,
                             18446744073709551615]
      val () = List.app rtS [0,1,~1,63,64,~64,~65,127,~128,128,~129,
                             2147483647,~2147483648,4294967296,
                             ~9223372036854775808,9223372036854775807]

      val () = section "leb128 decode position advance"
      val () = let val (v, i) = Leb128.decodeU (bytes [0xE5,0x8E,0x26,0xFF], 0)
               in checkLarge "u value" (624485, v); checkInt "u nextIdx" (3, i) end
      val () = checkRaises "decodeU truncated raises"
                 (fn () => Leb128.decodeU (bytes [0x80,0x80], 0))

      (* ============== Decode: committed add.wasm fixture ============== *)
      val () = section "decode (committed add.wasm)"
      val fileBytes = readBytes "test/fixtures/add.wasm"
      val () = check "fixture bytes == inline bytes" (fileBytes = addModuleBytes)
      val m = Decode.decode fileBytes
      val () = checkInt "types count" (1, length (#types m))
      val () = checkInt "funcs count" (1, length (#funcs m))
      val () = checkInt "exports count" (1, length (#exports m))
      val () = checkInt "no imported funcs" (0, #numImportedFuncs m)
      val ft = hd (#types m)
      val () = check "add type params" (#params ft = [I32T, I32T])
      val () = check "add type results" (#results ft = [I32T])
      val f = hd (#funcs m)
      val () = checkInt "add typeIdx" (0, #typeIdx f)
      val () = check "add no locals" (null (#locals f))
      val () = checkInstrs "add body"
                 ([LocalGet 0, LocalGet 1, I32Add], #body f)
      val ex = hd (#exports m)
      val () = checkString "export name" ("add", #name ex)
      val () = check "export is func" (#kind ex = FuncExport)
      val () = checkInt "export index" (0, #index ex)

      (* ============== Decode + Interp: add / sub / i64 ============== *)
      val () = section "interp on decoded binary modules"
      val () = checkVals "add(5,7) = 12"
                 ([I32 12], Interp.run (m, "add", [I32 5, I32 7]))
      val () = checkVals "add(-1,1) = 0"
                 ([I32 0], Interp.run (m, "add", [I32 (~1), I32 1]))
      val msub = Decode.decode subModuleBytes
      val () = checkVals "sub(10,3) = 7"
                 ([I32 7], Interp.run (msub, "sub", [I32 10, I32 3]))
      val mk = Decode.decode i64ModuleBytes
      val () = checkVals "k() = i64 2^32"
                 ([I64 4294967296], Interp.run (mk, "k", []))
      val () = check "i64 result type" (#results (hd (#types mk)) = [I64T])

      (* ============== Decode error handling ============== *)
      val () = section "decode errors"
      val () = checkRaises "bad magic raises"
                 (fn () => Decode.decode (bytes [0x00,0x61,0x73,0x00,
                                                 0x01,0x00,0x00,0x00]))
      val () = checkRaises "truncated header raises"
                 (fn () => Decode.decode (bytes [0x00,0x61,0x73]))
      val () = checkRaises "bad version raises"
                 (fn () => Decode.decode (bytes [0x00,0x61,0x73,0x6D,
                                                 0x02,0x00,0x00,0x00]))

      (* ============== Wat: committed math.wat fixture ============== *)
      val () = section "wat (committed math.wat)"
      val watText = readText "test/fixtures/math.wat"
      val wm = Wat.parse watText
      val () = checkInt "wat funcs" (5, length (#funcs wm))
      val () = checkInt "wat exports" (5, length (#exports wm))
      val () = checkVals "wat add(3,4) = 7"
                 ([I32 7], Interp.run (wm, "add", [I32 3, I32 4]))
      val () = checkVals "wat fib(10) = 55"
                 ([I32 55], Interp.run (wm, "fib", [I32 10]))
      val () = checkVals "wat fib(0) = 0"
                 ([I32 0], Interp.run (wm, "fib", [I32 0]))
      val () = checkVals "wat fib(1) = 1"
                 ([I32 1], Interp.run (wm, "fib", [I32 1]))
      val () = checkVals "wat fact(5) = 120"
                 ([I32 120], Interp.run (wm, "fact", [I32 5]))
      val () = checkVals "wat fact(0) = 1"
                 ([I32 1], Interp.run (wm, "fact", [I32 0]))
      val () = checkVals "wat fibrec(10) = 55"
                 ([I32 55], Interp.run (wm, "fibrec", [I32 10]))
      val () = checkVals "wat fibrec(13) = 233"
                 ([I32 233], Interp.run (wm, "fibrec", [I32 13]))
      val () = checkVals "wat mul64(1e6,1e6) = 1e12"
                 ([I64 1000000000000],
                  Interp.run (wm, "mul64", [I64 1000000, I64 1000000]))

      (* ============== Round trip: binary add == text add ============== *)
      val () = section "binary/text equivalence"
      val () = checkVals "binary add == wat add on (5,7)"
                 (Interp.run (m, "add", [I32 5, I32 7]),
                  Interp.run (wm, "add", [I32 5, I32 7]))

      (* ============== Wat: broader instruction coverage ============== *)
      val () = section "wat (instruction coverage)"
      val om = Wat.parse opsWat
      fun r2 (name, a, b) = Interp.run (om, name, [I32 a, I32 b])
      val () = checkVals "sub(10,3)"   ([I32 7],   r2 ("sub", 10, 3))
      val () = checkVals "divs(20,3)"  ([I32 6],   r2 ("divs", 20, 3))
      val () = checkVals "divs(-20,3)" ([I32 (~6)],r2 ("divs", ~20, 3))
      val () = checkVals "rems(20,3)"  ([I32 2],   r2 ("rems", 20, 3))
      val () = checkVals "rems(-20,3)" ([I32 (~2)],r2 ("rems", ~20, 3))
      val () = checkVals "and(12,10)"  ([I32 8],   r2 ("and", 12, 10))
      val () = checkVals "or(12,10)"   ([I32 14],  r2 ("or", 12, 10))
      val () = checkVals "xor(12,10)"  ([I32 6],   r2 ("xor", 12, 10))
      val () = checkVals "shl(1,4)"    ([I32 16],  r2 ("shl", 1, 4))
      val () = checkVals "shrs(-16,2)" ([I32 (~4)],r2 ("shrs", ~16, 2))
      val () = checkVals "shru(-1,28)" ([I32 15],  r2 ("shru", ~1, 28))
      val () = checkVals "lts(3,5)=1"  ([I32 1],   r2 ("lts", 3, 5))
      val () = checkVals "lts(5,3)=0"  ([I32 0],   r2 ("lts", 5, 3))
      val () = checkVals "eqz(0)=1"
                 ([I32 1], Interp.run (om, "eqz", [I32 0]))
      val () = checkVals "eqz(9)=0"
                 ([I32 0], Interp.run (om, "eqz", [I32 9]))
      val () = checkVals "sel(11,22,1)=11"
                 ([I32 11], Interp.run (om, "sel", [I32 11, I32 22, I32 1]))
      val () = checkVals "sel(11,22,0)=22"
                 ([I32 22], Interp.run (om, "sel", [I32 11, I32 22, I32 0]))
      val () = checkVals "drop2(9,8)=9"  ([I32 9], r2 ("drop2", 9, 8))
      val () = checkVals "tee -> 14"
                 ([I32 14], Interp.run (om, "tee", [I32 0]))
      val () = checkVals "addwrap32 -> -2^31"
                 ([I32 (~2147483648)], Interp.run (om, "addwrap32", []))
      val () = checkVals "mulwrap32 -> 0"
                 ([I32 0], Interp.run (om, "mulwrap32", []))
      val () = checkVals "addwrap64 -> -2^63"
                 ([I64 (~9223372036854775808)],
                  Interp.run (om, "addwrap64", []))
      val () = checkVals "div64(1e12,1e6) = 1e6"
                 ([I64 1000000],
                  Interp.run (om, "div64", [I64 1000000000000, I64 1000000]))

      (* ============== Globals (binary + text) ============== *)
      val () = section "globals"
      val gm = Decode.decode globalModuleBytes
      val () = checkInt "decoded globals count" (1, length (#globals gm))
      val () = checkVals "binary getg() = 42"
                 ([I32 42], Interp.run (gm, "getg", []))
      val () = checkVals "wat gget() = 100"
                 ([I32 100], Interp.run (om, "gget", []))
      val () = checkVals "wat gadd(5) = 105"
                 ([I32 105], Interp.run (om, "gadd", [I32 5]))

      (* ============== Interp trap handling ============== *)
      val () = section "interp traps"
      val () = checkRaises "div by zero traps"
                 (fn () => Interp.run (om, "divs", [I32 1, I32 0]))
      val () = checkRaises "unknown export traps"
                 (fn () => Interp.run (om, "nope", [I32 1, I32 0]))
    in () end

  fun run () = (reset (); runAll (); Harness.run ())
end
