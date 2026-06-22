# sml-wasm

Pure [Standard ML](https://en.wikipedia.org/wiki/Standard_ML) tooling for
[WebAssembly](https://webassembly.org/) — a **binary `.wasm` decoder**, a
**text `.wat` parser**, and a **stack-machine interpreter** — with **zero
dependencies**. Builds and tests cleanly under both [MLton](http://mlton.org/)
and [Poly/ML](https://www.polyml.org/), and is byte-for-byte deterministic
across the two.

## Overview

WebAssembly is a portable stack-machine bytecode. This library covers the
integer core of it, end to end:

- **`Decode`** — parses the binary module format: the `\0asm` magic + version-1
  header and the section sequence (type, import, function, table, memory,
  global, export, start, code; unknown/custom/data sections are skipped). It
  stitches the function section (type indices) and the code section (locals +
  bodies) together and produces a `module` AST.
- **`Wat`** — an s-expression parser for the text format covering the subset
  needed to express the supported instructions: `(module …)` with `(func …)`,
  `(global …)`, `(export …)` and `(start …)`. Functions use the *linear*
  instruction syntax (explicit `block`/`loop`/`if`/`else`/`end`); identifiers
  (`$name`) for functions, globals, locals and block labels are resolved to
  indices.
- **`Interp`** — a structured stack-machine interpreter. It executes an
  exported (or indexed) function over a list of argument values and returns the
  result value(s). Because it consumes the same `module` AST, binary and text
  modules run identically.

Integer arithmetic is done in arbitrary-precision `IntInf` and reduced to the
right bit width with two's-complement wrap-around, so results are identical
regardless of the host's native int size (MLton's `int` is 32-bit, so i64
magnitudes must use `LargeInt`/`IntInf`). LEB128 — the *standard* little-endian
variable-length integer encoding used by WebAssembly (unsigned for counts and
indices, signed for constants) — is provided by the `Leb128` structure.

## Install

Using [smlpkg](https://github.com/diku-dk/smlpkg):

```sh
smlpkg add github.com/sjqtentacles/sml-wasm
smlpkg sync
```

Then add the library to your MLB file:

```
$(SML_LIB)/basis/basis.mlb
lib/github.com/sjqtentacles/sml-wasm/sources.mlb
```

## Usage

```sml
(* Decode a binary module and run an exported function. *)
val bytes  = (* Word8Vector.vector holding a .wasm image *)
val m      = Wasm.decode bytes
val result = Wasm.run (m, "add", [Wasm.I32 5, Wasm.I32 7])   (* [I32 12] *)

(* Or parse the text format. *)
val src = "(module (func (export \"id\") (param i32) (result i32) local.get 0))"
val m2  = Wasm.parse src
val r2  = Wasm.run (m2, "id", [Wasm.I32 42])                 (* [I32 42] *)
```

The umbrella `Wasm` structure exposes:

```sml
signature WASM =
sig
  type module
  datatype value = I32 of Int32.int | I64 of LargeInt.int

  exception Decode of string
  exception Wat of string
  exception Trap of string

  val decode : Word8Vector.vector -> module          (* binary .wasm *)
  val parse  : string -> module                       (* text .wat   *)
  val run    : module * string * value list -> value list   (* by export name *)
  val invoke : module * int * value list -> value list      (* by index *)

  val valueToString  : value -> string
  val valuesToString : value list -> string
end
```

The lower-level structures `Leb128`, `Decode`, `Wat`, `Interp` and the shared
AST in `WasmAst` are all available directly as well.

## Supported instruction subset

Values are `i32` (held as `Int32.int`) and `i64` (held as `LargeInt.int`).
The interpreter implements:

- **Constants**: `i32.const`, `i64.const`
- **Locals / globals**: `local.get`, `local.set`, `local.tee`, `global.get`,
  `global.set`
- **Parametric**: `drop`, `select`
- **Control flow**: `block`, `loop`, `if`/`else`/`end`, `br`, `br_if`,
  `return`, `call`, `nop`, `unreachable`
- **i32 / i64 arithmetic**: `add`, `sub`, `mul`, `div_s`, `div_u`, `rem_s`,
  `rem_u`
- **i32 / i64 bitwise**: `and`, `or`, `xor`, `shl`, `shr_s`, `shr_u`, `rotl`,
  `rotr` (shift counts are masked mod 32 / 64)
- **i32 / i64 comparisons**: `eqz`, `eq`, `ne`, `lt_s`, `lt_u`, `gt_s`, `gt_u`,
  `le_s`, `le_u`, `ge_s`, `ge_u`

Arithmetic wraps with two's-complement semantics (e.g. `i32` `2147483647 + 1`
= `-2147483648`); `div_s`/`rem_s` truncate toward zero and `rem_s` takes the
sign of the dividend; division by zero, signed division overflow
(`INT_MIN / -1`) and `unreachable` raise `Interp.Trap`.

Out of scope (not implemented): floating point (`f32`/`f64`), linear-memory
load/store, `br_table`, `call_indirect`, and the numeric conversion
instructions. The text parser does not accept the folded (fully-parenthesised)
instruction form or `(import …)`.

## Example

Running `make example` (see [`examples/demo.sml`](examples/demo.sml)) prints:

```
== LEB128 ==
unsigned 624485 -> [0xE5 0x8E 0x26]
unsigned 128    -> [0x80 0x01]
signed   -1     -> [0x7F]
signed   -64    -> [0x40]

== Decode (binary .wasm) ==
module bytes: 41, exports: add
add(5, 7)   = [i32:12]
add(-1, 100)= [i32:99]

== Wat (text .wat) + interpret ==
fib(10)     = [i32:55]
fib(20)     = [i32:6765]
fact(5)     = [i32:120]
fact(10)    = [i32:3628800]
mul64(1e6,1e6) = [i64:1000000000000]
```

## Testing

The test suite follows strict TDD (written before the implementation). It
covers LEB128 reference vectors and round-trips, a committed binary fixture
([`test/fixtures/add.wasm`](test/fixtures/add.wasm)) and a committed text
fixture ([`test/fixtures/math.wat`](test/fixtures/math.wat)) read at test time,
hand-built inline byte modules, AST-shape assertions, malformed-input errors,
runtime traps, and a broad sweep of the instruction set (iterative `fib`/`fact`
loops, recursive `fib` exercising `call`/`if`/`else`, i32/i64 wrap-around,
shifts, comparisons, `select`, globals, …).

```sh
make test        # MLton
make test-poly   # Poly/ML
make all-tests   # both
make example     # run the demo
```

Both compilers report `105 passed, 0 failed`, with byte-identical output.

## Layout

```
lib/github.com/sjqtentacles/sml-wasm/
  types.sml      shared AST + value types (WasmAst)
  leb128.{sig,sml}   LEB128 codec (Leb128)
  decode.{sig,sml}   binary .wasm decoder (Decode)
  wat.{sig,sml}      text .wat parser (Wat)
  interp.{sig,sml}   stack-machine interpreter (Interp)
  wasm.{sig,sml}     umbrella structure (Wasm)
```

## License

MIT.
