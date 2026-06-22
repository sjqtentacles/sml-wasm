(* interp.sml — a structured stack-machine interpreter.

   Integer arithmetic is performed in arbitrary-precision IntInf and then
   reduced to the appropriate bit width with two's-complement wrap-around, so
   results are identical regardless of the host compiler's native int size
   (MLton's int is 32-bit).  Control flow (block / loop / if / br / br_if /
   return) is implemented with a [ctrl] signal that unwinds the relevant
   number of structured-control frames. *)
structure Interp :> INTERP =
struct
  open WasmAst
  exception Trap of string

  (* ---- bit-width helpers (all on IntInf) ---- *)
  val p32 = IntInf.<< (1, 0w32)
  val p31 = IntInf.<< (1, 0w31)
  val p64 = IntInf.<< (1, 0w64)
  val p63 = IntInf.<< (1, 0w63)

  fun u32 x = IntInf.mod (x, p32)               (* unsigned 32-bit value *)
  fun u64 x = IntInf.mod (x, p64)
  fun s32 x = let val u = u32 x in if u >= p31 then u - p32 else u end
  fun s64 x = let val u = u64 x in if u >= p63 then u - p64 else u end

  fun mkI32 (n : IntInf.int) = I32 (Int32.fromLarge (s32 n))
  fun mkI64 (n : IntInf.int) = I64 (s64 n)

  fun cnt32 b = IntInf.toInt (IntInf.mod (u32 b, 32))
  fun cnt64 b = IntInf.toInt (IntInf.mod (u64 b, 64))

  (* explicit rotates (width-specific; result reduced by mkI32/mkI64) *)
  fun rotl32 (a, b) =
    let val x = u32 a val c = cnt32 b in
      if c = 0 then x
      else IntInf.orb (IntInf.<< (x, Word.fromInt c),
                       IntInf.~>> (x, Word.fromInt (32 - c)))
    end
  fun rotr32 (a, b) =
    let val x = u32 a val c = cnt32 b in
      if c = 0 then x
      else IntInf.orb (IntInf.~>> (x, Word.fromInt c),
                       IntInf.<< (x, Word.fromInt (32 - c)))
    end
  fun rotl64 (a, b) =
    let val x = u64 a val c = cnt64 b in
      if c = 0 then x
      else IntInf.orb (IntInf.<< (x, Word.fromInt c),
                       IntInf.~>> (x, Word.fromInt (64 - c)))
    end
  fun rotr64 (a, b) =
    let val x = u64 a val c = cnt64 b in
      if c = 0 then x
      else IntInf.orb (IntInf.~>> (x, Word.fromInt c),
                       IntInf.<< (x, Word.fromInt (64 - c)))
    end

  fun asI32 (I32 n) = Int32.toLarge n
    | asI32 _ = raise Trap "expected an i32 value"
  fun asI64 (I64 n) = n
    | asI64 _ = raise Trap "expected an i64 value"

  (* A module instance: the module plus its mutable global store. *)
  type instance = { m : module, globals : value array }

  fun makeInstance (m : module) : instance =
    let
      val arr = Array.array (length (#globals m), I32 0)
      fun ev (instrs, gi) =
        case instrs of
          [I32Const k] => I32 k
        | [I64Const k] => I64 k
        | [GlobalGet j] => if j < gi then Array.sub (arr, j)
                           else raise Trap "global initializer references later global"
        | _ => raise Trap "unsupported global initializer"
      fun fill (_, []) = ()
        | fill (gi, g :: gs) =
            (Array.update (arr, gi, ev (#init g, gi)); fill (gi + 1, gs))
      val () = fill (0, #globals m)
    in { m = m, globals = arr } end

  datatype ctrl = Next | Branch of int | Ret

  fun execFn (inst : instance) (funcIdx, args) : value list =
    let
      val m    = #m inst
      val nImp  = #numImportedFuncs m
      val ()    = if funcIdx < nImp
                  then raise Trap "call to imported function unsupported"
                  else ()
      val f  = List.nth (#funcs m, funcIdx - nImp)
               handle Subscript => raise Trap "function index out of range"
      val ft = List.nth (#types m, #typeIdx f)
               handle Subscript => raise Trap "type index out of range"
      val nparams = length (#params ft)
      val () = if length args = nparams then ()
               else raise Trap "argument count mismatch"

      fun zero I32T = I32 0 | zero I64T = I64 0
      val locals = Array.fromList (args @ List.map zero (#locals f))
      val stack  = ref ([] : value list)

      fun push v = stack := v :: !stack
      fun pop () =
        case !stack of x :: xs => (stack := xs; x)
                     | [] => raise Trap "operand stack underflow"
      fun popI32 () = asI32 (pop ())
      fun popI64 () = asI64 (pop ())

      fun funcTypeAt fidx =
        if fidx < nImp then raise Trap "call to imported function unsupported"
        else
          let val g = List.nth (#funcs m, fidx - nImp)
                      handle Subscript => raise Trap "function index out of range"
          in List.nth (#types m, #typeIdx g)
             handle Subscript => raise Trap "type index out of range" end

      fun i32bin g = let val b = popI32 () val a = popI32 () in push (mkI32 (g (a, b))) end
      fun i64bin g = let val b = popI64 () val a = popI64 () in push (mkI64 (g (a, b))) end
      fun i32cmp g = let val b = popI32 () val a = popI32 ()
                     in push (mkI32 (if g (a, b) then 1 else 0)) end
      fun i64cmp g = let val b = popI64 () val a = popI64 ()
                     in push (mkI32 (if g (a, b) then 1 else 0)) end

      fun numeric instr =
        case instr of
        (* i32 arithmetic *)
          I32Add  => i32bin (fn (a, b) => a + b)
        | I32Sub  => i32bin (fn (a, b) => a - b)
        | I32Mul  => i32bin (fn (a, b) => a * b)
        | I32DivS =>
            let val b = popI32 () val a = popI32 () in
              if b = 0 then raise Trap "integer divide by zero"
              else if a = ~p31 andalso b = ~1 then raise Trap "integer overflow"
              else push (mkI32 (IntInf.quot (a, b)))
            end
        | I32DivU =>
            let val b = u32 (popI32 ()) val a = u32 (popI32 ()) in
              if b = 0 then raise Trap "integer divide by zero"
              else push (mkI32 (IntInf.quot (a, b)))
            end
        | I32RemS =>
            let val b = popI32 () val a = popI32 () in
              if b = 0 then raise Trap "integer divide by zero"
              else push (mkI32 (IntInf.rem (a, b)))
            end
        | I32RemU =>
            let val b = u32 (popI32 ()) val a = u32 (popI32 ()) in
              if b = 0 then raise Trap "integer divide by zero"
              else push (mkI32 (IntInf.rem (a, b)))
            end
        | I32And  => i32bin (fn (a, b) => IntInf.andb (u32 a, u32 b))
        | I32Or   => i32bin (fn (a, b) => IntInf.orb  (u32 a, u32 b))
        | I32Xor  => i32bin (fn (a, b) => IntInf.xorb (u32 a, u32 b))
        | I32Shl  => i32bin (fn (a, b) => IntInf.<<  (u32 a, Word.fromInt (cnt32 b)))
        | I32ShrU => i32bin (fn (a, b) => IntInf.~>> (u32 a, Word.fromInt (cnt32 b)))
        | I32ShrS => i32bin (fn (a, b) => IntInf.~>> (s32 a, Word.fromInt (cnt32 b)))
        | I32Rotl => i32bin rotl32
        | I32Rotr => i32bin rotr32
        (* i32 comparisons *)
        | I32Eqz  => let val a = popI32 () in push (mkI32 (if a = 0 then 1 else 0)) end
        | I32Eq   => i32cmp (fn (a, b) => a = b)
        | I32Ne   => i32cmp (fn (a, b) => a <> b)
        | I32LtS  => i32cmp (fn (a, b) => a < b)
        | I32GtS  => i32cmp (fn (a, b) => a > b)
        | I32LeS  => i32cmp (fn (a, b) => a <= b)
        | I32GeS  => i32cmp (fn (a, b) => a >= b)
        | I32LtU  => i32cmp (fn (a, b) => u32 a < u32 b)
        | I32GtU  => i32cmp (fn (a, b) => u32 a > u32 b)
        | I32LeU  => i32cmp (fn (a, b) => u32 a <= u32 b)
        | I32GeU  => i32cmp (fn (a, b) => u32 a >= u32 b)
        (* i64 arithmetic *)
        | I64Add  => i64bin (fn (a, b) => a + b)
        | I64Sub  => i64bin (fn (a, b) => a - b)
        | I64Mul  => i64bin (fn (a, b) => a * b)
        | I64DivS =>
            let val b = popI64 () val a = popI64 () in
              if b = 0 then raise Trap "integer divide by zero"
              else if a = ~p63 andalso b = ~1 then raise Trap "integer overflow"
              else push (mkI64 (IntInf.quot (a, b)))
            end
        | I64DivU =>
            let val b = u64 (popI64 ()) val a = u64 (popI64 ()) in
              if b = 0 then raise Trap "integer divide by zero"
              else push (mkI64 (IntInf.quot (a, b)))
            end
        | I64RemS =>
            let val b = popI64 () val a = popI64 () in
              if b = 0 then raise Trap "integer divide by zero"
              else push (mkI64 (IntInf.rem (a, b)))
            end
        | I64RemU =>
            let val b = u64 (popI64 ()) val a = u64 (popI64 ()) in
              if b = 0 then raise Trap "integer divide by zero"
              else push (mkI64 (IntInf.rem (a, b)))
            end
        | I64And  => i64bin (fn (a, b) => IntInf.andb (u64 a, u64 b))
        | I64Or   => i64bin (fn (a, b) => IntInf.orb  (u64 a, u64 b))
        | I64Xor  => i64bin (fn (a, b) => IntInf.xorb (u64 a, u64 b))
        | I64Shl  => i64bin (fn (a, b) => IntInf.<<  (u64 a, Word.fromInt (cnt64 b)))
        | I64ShrU => i64bin (fn (a, b) => IntInf.~>> (u64 a, Word.fromInt (cnt64 b)))
        | I64ShrS => i64bin (fn (a, b) => IntInf.~>> (s64 a, Word.fromInt (cnt64 b)))
        | I64Rotl => i64bin rotl64
        | I64Rotr => i64bin rotr64
        (* i64 comparisons (result is i32) *)
        | I64Eqz  => let val a = popI64 () in push (mkI32 (if a = 0 then 1 else 0)) end
        | I64Eq   => i64cmp (fn (a, b) => a = b)
        | I64Ne   => i64cmp (fn (a, b) => a <> b)
        | I64LtS  => i64cmp (fn (a, b) => a < b)
        | I64GtS  => i64cmp (fn (a, b) => a > b)
        | I64LeS  => i64cmp (fn (a, b) => a <= b)
        | I64GeS  => i64cmp (fn (a, b) => a >= b)
        | I64LtU  => i64cmp (fn (a, b) => u64 a < u64 b)
        | I64GtU  => i64cmp (fn (a, b) => u64 a > u64 b)
        | I64LeU  => i64cmp (fn (a, b) => u64 a <= u64 b)
        | I64GeU  => i64cmp (fn (a, b) => u64 a >= u64 b)
        | _ => raise Trap "internal: non-numeric instruction"

      fun execSeq [] = Next
        | execSeq (i :: rest) =
            (case execOne i of Next => execSeq rest | other => other)

      and execOne instr =
        case instr of
          Unreachable => raise Trap "unreachable"
        | Nop => Next
        | Drop => (ignore (pop ()); Next)
        | Select =>
            let val c = popI32 () val v2 = pop () val v1 = pop ()
            in push (if c <> 0 then v1 else v2); Next end
        | LocalGet i =>
            (push (Array.sub (locals, i)
                   handle Subscript => raise Trap "local index out of range"); Next)
        | LocalSet i =>
            ((Array.update (locals, i, pop ())
              handle Subscript => raise Trap "local index out of range"); Next)
        | LocalTee i =>
            let val v = pop () in
              (Array.update (locals, i, v)
               handle Subscript => raise Trap "local index out of range");
              push v; Next
            end
        | GlobalGet i =>
            (push (Array.sub (#globals inst, i)
                   handle Subscript => raise Trap "global index out of range"); Next)
        | GlobalSet i =>
            ((Array.update (#globals inst, i, pop ())
              handle Subscript => raise Trap "global index out of range"); Next)
        | I32Const k => (push (I32 k); Next)
        | I64Const k => (push (I64 k); Next)
        | Block (_, body) =>
            (case execSeq body of
               Branch 0 => Next | Branch n => Branch (n - 1) | other => other)
        | Loop (_, body) =>
            let fun iter () =
                  case execSeq body of
                    Branch 0 => iter ()
                  | Branch n => Branch (n - 1)
                  | other => other
            in iter () end
        | If (_, thn, els) =>
            let val c = popI32 () in
              case execSeq (if c <> 0 then thn else els) of
                Branch 0 => Next | Branch n => Branch (n - 1) | other => other
            end
        | Br n => Branch n
        | BrIf n => if popI32 () <> 0 then Branch n else Next
        | Return => Ret
        | Call fidx =>
            let
              val ft' = funcTypeAt fidx
              val nargs = length (#params ft')
              fun take (0, acc) = acc
                | take (k, acc) = take (k - 1, pop () :: acc)
              val callArgs = take (nargs, [])
              val results = execFn inst (fidx, callArgs)
            in List.app push results; Next end
        | other => (numeric other; Next)

      val _ = execSeq (#body f)
      val nres = length (#results ft)
      fun takeN (0, _, acc) = acc
        | takeN (k, x :: xs, acc) = takeN (k - 1, xs, x :: acc)
        | takeN (_, [], _) = raise Trap "missing result values on stack"
    in
      takeN (nres, !stack, [])
    end

  fun invoke (m, funcIdx, args) = execFn (makeInstance m) (funcIdx, args)

  fun run (m, name, args) =
    case List.find (fn e => #name e = name andalso #kind e = FuncExport)
                   (#exports m) of
      SOME e => invoke (m, #index e, args)
    | NONE => raise Trap ("no exported function named \"" ^ name ^ "\"")
end
