(* demo.sml — a deterministic tour of sml-wasm.

   Run with `make example`.  Everything here is self-contained (no external
   files), so the output is identical on MLton and Poly/ML. *)

fun bytes xs = Word8Vector.fromList (List.map Word8.fromInt xs)

fun ints v = Word8Vector.foldr (fn (b, a) => Word8.toInt b :: a) [] v
fun showBytes v =
  "[" ^ String.concatWith " "
          (List.map (fn b => "0x" ^ StringCvt.padLeft #"0" 2
                                       (Int.fmt StringCvt.HEX b)) (ints v)) ^ "]"

(* A hand-assembled binary module exporting add : (i32,i32) -> i32. *)
val addWasm = bytes
  [ 0x00,0x61,0x73,0x6D, 0x01,0x00,0x00,0x00,
    0x01,0x07,0x01,0x60,0x02,0x7F,0x7F,0x01,0x7F,
    0x03,0x02,0x01,0x00,
    0x07,0x07,0x01,0x03,0x61,0x64,0x64,0x00,0x00,
    0x0A,0x09,0x01,0x07,0x00,0x20,0x00,0x20,0x01,0x6A,0x0B ]

(* A text module: iterative fibonacci and factorial, plus a 64-bit multiply. *)
val mathWat =
  "(module\n\
  \  (func (export \"fib\") (param $n i32) (result i32)\n\
  \    (local $a i32) (local $b i32) (local $i i32)\n\
  \    i32.const 0 local.set $a\n\
  \    i32.const 1 local.set $b\n\
  \    block $done\n\
  \      loop $cont\n\
  \        local.get $i local.get $n i32.ge_s br_if $done\n\
  \        local.get $a local.get $b i32.add\n\
  \        local.get $b local.set $a local.set $b\n\
  \        local.get $i i32.const 1 i32.add local.set $i\n\
  \        br $cont\n\
  \      end\n\
  \    end\n\
  \    local.get $a)\n\
  \  (func (export \"fact\") (param $n i32) (result i32)\n\
  \    (local $acc i32) (local $i i32)\n\
  \    i32.const 1 local.set $acc\n\
  \    i32.const 1 local.set $i\n\
  \    block $done\n\
  \      loop $cont\n\
  \        local.get $i local.get $n i32.gt_s br_if $done\n\
  \        local.get $acc local.get $i i32.mul local.set $acc\n\
  \        local.get $i i32.const 1 i32.add local.set $i\n\
  \        br $cont\n\
  \      end\n\
  \    end\n\
  \    local.get $acc)\n\
  \  (func (export \"mul64\") (param i64) (param i64) (result i64)\n\
  \    local.get 0 local.get 1 i64.mul))\n"

fun line s = print (s ^ "\n")

val () = line "== LEB128 =="
val () = line ("unsigned 624485 -> " ^ showBytes (Leb128.encodeU 624485))
val () = line ("unsigned 128    -> " ^ showBytes (Leb128.encodeU 128))
val () = line ("signed   -1     -> " ^ showBytes (Leb128.encodeS (~1)))
val () = line ("signed   -64    -> " ^ showBytes (Leb128.encodeS (~64)))

val () = line ""
val () = line "== Decode (binary .wasm) =="
val addModule = Wasm.decode addWasm
val () = line ("module bytes: " ^ Int.toString (Word8Vector.length addWasm)
               ^ ", exports: "
               ^ String.concatWith ", " (List.map #name (#exports addModule)))
val () = line ("add(5, 7)   = "
               ^ Wasm.valuesToString (Wasm.run (addModule, "add", [Wasm.I32 5, Wasm.I32 7])))
val () = line ("add(-1, 100)= "
               ^ Wasm.valuesToString (Wasm.run (addModule, "add", [Wasm.I32 (~1), Wasm.I32 100])))

val () = line ""
val () = line "== Wat (text .wat) + interpret =="
val mathModule = Wasm.parse mathWat
fun fib n = Wasm.run (mathModule, "fib", [Wasm.I32 n])
fun fact n = Wasm.run (mathModule, "fact", [Wasm.I32 n])
val () = line ("fib(10)     = " ^ Wasm.valuesToString (fib 10))
val () = line ("fib(20)     = " ^ Wasm.valuesToString (fib 20))
val () = line ("fact(5)     = " ^ Wasm.valuesToString (fact 5))
val () = line ("fact(10)    = " ^ Wasm.valuesToString (fact 10))
val () = line ("mul64(1e6,1e6) = "
               ^ Wasm.valuesToString
                   (Wasm.run (mathModule, "mul64",
                              [Wasm.I64 1000000, Wasm.I64 1000000])))
