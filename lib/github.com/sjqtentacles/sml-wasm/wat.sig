(* wat.sig — WebAssembly text format (`.wat`) parser.

   An s-expression parser covering the subset needed to express the supported
   instruction set: `(module ...)` containing `(func ...)`, `(global ...)`,
   `(export ...)` and `(start ...)` fields.  Functions use the *linear*
   instruction syntax (explicit `block`/`loop`/`if`/`else`/`end`) rather than
   the folded form.  Named identifiers (`$name`) for functions, locals and
   block labels are resolved to indices.

   Numeric index literals (function/global/local/label references and export
   targets) are range-checked against a fixed portable bound (0 .. 2^31-1) via
   arbitrary-precision scanning, so an out-of-range index yields a `Wat`
   failure rather than a leaked `Overflow` — identical on 32-bit-int MLton and
   63-bit Poly/ML. *)
signature WAT =
sig
  (* Raised with a human-readable reason on a parse/resolution error, including
     a malformed or out-of-range (> 2^31-1) numeric index literal. *)
  exception Wat of string

  (* Parse a `.wat` source string into a module AST. *)
  val parse : string -> WasmAst.module
end
