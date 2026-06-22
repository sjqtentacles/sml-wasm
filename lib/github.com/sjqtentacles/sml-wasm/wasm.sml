(* wasm.sml — umbrella structure re-exporting the public API. *)
structure Wasm :> WASM =
struct
  type module = WasmAst.module
  datatype value = datatype WasmAst.value

  exception Decode = Decode.Decode
  exception Wat    = Wat.Wat
  exception Trap   = Interp.Trap

  val decode = Decode.decode
  val parse  = Wat.parse
  val run    = Interp.run
  val invoke = Interp.invoke

  val valueToString  = WasmAst.valueToString
  val valuesToString = WasmAst.valuesToString
end
