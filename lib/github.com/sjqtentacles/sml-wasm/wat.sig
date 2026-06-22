(* wat.sig — WebAssembly text format (`.wat`) parser.

   An s-expression parser covering the subset needed to express the supported
   instruction set: `(module ...)` containing `(func ...)`, `(global ...)`,
   `(export ...)` and `(start ...)` fields.  Functions use the *linear*
   instruction syntax (explicit `block`/`loop`/`if`/`else`/`end`) rather than
   the folded form.  Named identifiers (`$name`) for functions, locals and
   block labels are resolved to indices. *)
signature WAT =
sig
  (* Raised with a human-readable reason on a parse/resolution error. *)
  exception Wat of string

  (* Parse a `.wat` source string into a module AST. *)
  val parse : string -> WasmAst.module
end
