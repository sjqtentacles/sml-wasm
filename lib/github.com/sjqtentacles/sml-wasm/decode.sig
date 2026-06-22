(* decode.sig — binary `.wasm` module decoder.

   Parses the WebAssembly binary format: the `\0asm` magic and version-1
   header, then the section sequence (type, import, function, table, memory,
   global, export, start, code, plus skipped custom/other sections) into a
   [WasmAst.module]. *)
signature DECODE =
sig
  (* Raised with a human-readable reason on malformed/unsupported input. *)
  exception Decode of string

  (* Decode a complete binary module image. *)
  val decode : Word8Vector.vector -> WasmAst.module
end
