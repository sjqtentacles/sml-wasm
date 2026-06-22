(* interp.sig — a stack-machine interpreter for decoded/parsed modules.

   Executes an exported (or indexed) function over a list of argument values,
   returning the result value(s).  Operates on the same [WasmAst.module]
   produced by either [Decode] or [Wat], so binary and text inputs run
   identically. *)
signature INTERP =
sig
  (* Raised on a runtime trap: stack/type misuse, division by zero, signed
     division overflow, out-of-range index, unsupported instruction, etc. *)
  exception Trap of string

  (* [run (m, name, args)] invokes the exported function called [name]. *)
  val run : WasmAst.module * string * WasmAst.value list -> WasmAst.value list

  (* [invoke (m, funcIdx, args)] invokes a function by its index in the
     function index space. *)
  val invoke : WasmAst.module * int * WasmAst.value list -> WasmAst.value list
end
