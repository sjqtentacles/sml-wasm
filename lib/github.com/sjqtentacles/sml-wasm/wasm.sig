(* wasm.sig — umbrella signature re-exporting the public API.

   Convenience surface that bundles the AST types and the three engines so a
   client can `open Wasm` and reach everything through one structure. *)
signature WASM =
sig
  (* AST type aliases (transparent; same types as structure WasmAst). *)
  type module = WasmAst.module
  datatype value = datatype WasmAst.value

  exception Decode of string
  exception Wat of string
  exception Trap of string

  (* Binary `.wasm` decoder. *)
  val decode : Word8Vector.vector -> module
  (* Text `.wat` parser. *)
  val parse  : string -> module
  (* Run an exported function by name. *)
  val run    : module * string * value list -> value list
  (* Run a function by index. *)
  val invoke : module * int * value list -> value list

  val valueToString  : value -> string
  val valuesToString : value list -> string
end
