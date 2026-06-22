(* types.sml — shared WebAssembly AST and value types.

   This structure is intentionally *not* opaquely sealed: its datatypes are
   the common currency shared between the LEB128 codec, the binary `Decode`r,
   the text (`Wat`) parser, and the `Interp`reter, so their constructors must
   be visible to every other module and to the test suite. *)
structure WasmAst =
struct
  (* Value types we support.  WebAssembly also has f32/f64 and the reference
     types; this library handles the two integer types only. *)
  datatype valtype = I32T | I64T

  (* A runtime value.  i32 is held as a (signed) Int32.int; i64 is held as a
     signed LargeInt.int constrained to the 64-bit range.  MLton's default int
     is 32-bit, so i64 magnitudes must use LargeInt/IntInf. *)
  datatype value = I32 of Int32.int
                 | I64 of LargeInt.int

  (* Block signature: empty, a single result value type, or a type index
     (rare; carried so the binary decoder can round-trip it). *)
  datatype blocktype = BTEmpty
                     | BTVal of valtype
                     | BTType of int

  (* The supported instruction set.  See README for the exact opcode list. *)
  datatype instr =
      Unreachable
    | Nop
    | Block of blocktype * instr list
    | Loop  of blocktype * instr list
    | If    of blocktype * instr list * instr list   (* then / else *)
    | Br    of int
    | BrIf  of int
    | Return
    | Call  of int
    | Drop
    | Select
    | LocalGet  of int
    | LocalSet  of int
    | LocalTee  of int
    | GlobalGet of int
    | GlobalSet of int
    | I32Const of Int32.int
    | I64Const of LargeInt.int
    (* i32 comparisons *)
    | I32Eqz | I32Eq | I32Ne
    | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS | I32GeU
    (* i64 comparisons *)
    | I64Eqz | I64Eq | I64Ne
    | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS | I64GeU
    (* i32 arithmetic / bitwise *)
    | I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU
    | I32And | I32Or | I32Xor | I32Shl | I32ShrS | I32ShrU | I32Rotl | I32Rotr
    (* i64 arithmetic / bitwise *)
    | I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU
    | I64And | I64Or | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Rotl | I64Rotr

  type functype = { params : valtype list, results : valtype list }

  (* A defined function: index into the module's type vector, the locally
     declared (non-parameter) locals, and the instruction body. *)
  type func = { typeIdx : int, locals : valtype list, body : instr list }

  datatype mut = Const | Var
  type global = { typ : valtype, mut : mut, init : instr list }

  datatype exportkind = FuncExport | TableExport | MemExport | GlobalExport
  type export = { name : string, kind : exportkind, index : int }

  (* A decoded / parsed module.  [numImportedFuncs] records how many entries
     at the front of the function index space are imported (so a defined
     function at function index i lives at funcs[i - numImportedFuncs]). *)
  type module =
    { types            : functype list
    , funcs            : func list
    , globals          : global list
    , exports          : export list
    , start            : int option
    , numImportedFuncs : int }

  (* ---- small helpers ---- *)

  fun valtypeToString I32T = "i32"
    | valtypeToString I64T = "i64"

  (* Render an integer with a leading '-' (not SML's '~') for negatives, so
     output is uniform and friendly across both compilers. *)
  fun fixSign s =
    if String.isPrefix "~" s then "-" ^ String.extract (s, 1, NONE) else s

  fun valueToString (I32 n) = "i32:" ^ fixSign (Int32.toString n)
    | valueToString (I64 n) = "i64:" ^ fixSign (LargeInt.toString n)

  fun valuesToString vs =
    "[" ^ String.concatWith ", " (List.map valueToString vs) ^ "]"
end
