(* wat.sml — WebAssembly text format (`.wat`) parser.

   A two-layer parser: a tokenizer (handling `;;` line comments, nested
   `(; ;)` block comments and `"..."` string literals) feeds an s-expression
   reader; the s-expressions are then interpreted as a module.  Functions use
   *linear* instruction syntax (explicit `block`/`loop`/`if`/`else`/`end`);
   folded instruction forms are rejected.  Identifiers (`$name`) for
   functions, globals, locals and block labels are resolved to indices. *)
structure Wat :> WAT =
struct
  open WasmAst
  exception Wat of string

  datatype tok  = LP | RP | ATOM of string | STR of string
  datatype sexp = A of string | S of string | L of sexp list

  val p32 = IntInf.<< (1, 0w32)  val p31 = IntInf.<< (1, 0w31)
  val p64 = IntInf.<< (1, 0w64)  val p63 = IntInf.<< (1, 0w63)
  fun toI32 n =
    let val m = IntInf.mod (n, p32) in Int32.fromLarge (if m >= p31 then m - p32 else m) end
  fun toI64 n =
    let val m = IntInf.mod (n, p64) in if m >= p63 then m - p64 else m end

  fun parseIntInf str =
    let
      val (neg, s) = if String.isPrefix "-" str
                     then (true, String.extract (str, 1, NONE))
                     else if String.isPrefix "+" str
                       then (false, String.extract (str, 1, NONE))
                     else (false, str)
      val (radix, digits) =
        if String.isPrefix "0x" s orelse String.isPrefix "0X" s
        then (StringCvt.HEX, String.extract (s, 2, NONE))
        else (StringCvt.DEC, s)
      val v = case StringCvt.scanString (IntInf.scan radix) digits of
                SOME x => x
              | NONE => raise Wat ("bad integer literal: " ^ str)
    in if neg then ~v else v end

  (* ---- tokenizer ---- *)
  fun tokenize s =
    let
      val n = String.size s
      fun isDelim c = c = #"(" orelse c = #")" orelse Char.isSpace c
      fun lp (i, acc) =
        if i >= n then List.rev acc
        else
          let val c = String.sub (s, i) in
            if Char.isSpace c then lp (i + 1, acc)
            else if c = #";" andalso i + 1 < n andalso String.sub (s, i + 1) = #";" then
              let fun skip j = if j >= n orelse String.sub (s, j) = #"\n"
                               then j else skip (j + 1)
              in lp (skip (i + 2), acc) end
            else if c = #"(" andalso i + 1 < n andalso String.sub (s, i + 1) = #";" then
              let fun skip (j, depth) =
                    if j + 1 >= n then n
                    else if String.sub (s, j) = #"(" andalso String.sub (s, j + 1) = #";"
                      then skip (j + 2, depth + 1)
                    else if String.sub (s, j) = #";" andalso String.sub (s, j + 1) = #")"
                      then if depth = 1 then j + 2 else skip (j + 2, depth - 1)
                    else skip (j + 1, depth)
              in lp (skip (i + 2, 1), acc) end
            else if c = #"(" then lp (i + 1, LP :: acc)
            else if c = #")" then lp (i + 1, RP :: acc)
            else if c = #"\"" then
              let
                fun rd (j, cs) =
                  if j >= n then raise Wat "unterminated string literal"
                  else let val d = String.sub (s, j) in
                    if d = #"\"" then (j + 1, String.implode (List.rev cs))
                    else if d = #"\\" andalso j + 1 < n then
                      let val e = String.sub (s, j + 1)
                          val ch = case e of #"n" => #"\n" | #"t" => #"\t"
                                          | #"r" => #"\r" | _ => e
                      in rd (j + 2, ch :: cs) end
                    else rd (j + 1, d :: cs)
                  end
                val (j, str) = rd (i + 1, [])
              in lp (j, STR str :: acc) end
            else
              let
                fun rd j =
                  if j >= n orelse isDelim (String.sub (s, j))
                     orelse String.sub (s, j) = #";"
                  then j else rd (j + 1)
                val j = rd i
              in
                if j = i then lp (i + 1, acc)   (* skip stray char, never loop *)
                else lp (j, ATOM (String.substring (s, i, j - i)) :: acc)
              end
          end
    in lp (0, []) end

  (* ---- s-expression reader ---- *)
  fun parseSexps tokens =
    let
      fun list (toks, acc) =
        case toks of
          [] => raise Wat "unexpected end of input (missing ')')"
        | RP :: rest => (List.rev acc, rest)
        | LP :: rest => let val (sub, rest') = list (rest, [])
                        in list (rest', L sub :: acc) end
        | ATOM a :: rest => list (rest, A a :: acc)
        | STR x :: rest  => list (rest, S x :: acc)
      fun top (toks, acc) =
        case toks of
          [] => List.rev acc
        | LP :: rest => let val (sub, rest') = list (rest, [])
                        in top (rest', L sub :: acc) end
        | RP :: _ => raise Wat "unexpected ')'"
        | ATOM a :: rest => top (rest, A a :: acc)
        | STR x :: rest  => top (rest, S x :: acc)
    in top (tokens, []) end

  fun parseValtype (A "i32") = I32T
    | parseValtype (A "i64") = I64T
    | parseValtype _ = raise Wat "unsupported value type (only i32/i64)"

  (* parse the items of a (param ...) / (local ...) into (name?, type) pairs *)
  fun parseNamedTypes items =
    case items of
      (A nm) :: rest =>
        if String.isPrefix "$" nm
        then (case rest of t :: _ => [(SOME nm, parseValtype t)]
                         | [] => raise Wat "named param/local without a type")
        else List.map (fn it => (NONE, parseValtype it)) items
    | _ => List.map (fn it => (NONE, parseValtype it)) items

  (* split leading (param)/(result)/(local) decls from the instruction items *)
  fun gather (items, ps, rs, ls) =
    case items of
      (L (A "param" :: rest)) :: more  => gather (more, ps @ parseNamedTypes rest, rs, ls)
    | (L (A "result" :: rest)) :: more => gather (more, ps, rs @ List.map parseValtype rest, ls)
    | (L (A "local" :: rest)) :: more  => gather (more, ps, rs, ls @ parseNamedTypes rest)
    | _ => (ps, rs, ls, items)

  fun labelIndex (labels, name) =
    let fun go (_, []) = NONE
          | go (i, x :: xs) = if x = SOME name then SOME i else go (i + 1, xs)
    in go (0, labels) end

  fun simpleOp s =
    case s of
      "i32.add" => SOME I32Add | "i32.sub" => SOME I32Sub | "i32.mul" => SOME I32Mul
    | "i32.div_s" => SOME I32DivS | "i32.div_u" => SOME I32DivU
    | "i32.rem_s" => SOME I32RemS | "i32.rem_u" => SOME I32RemU
    | "i32.and" => SOME I32And | "i32.or" => SOME I32Or | "i32.xor" => SOME I32Xor
    | "i32.shl" => SOME I32Shl | "i32.shr_s" => SOME I32ShrS | "i32.shr_u" => SOME I32ShrU
    | "i32.rotl" => SOME I32Rotl | "i32.rotr" => SOME I32Rotr
    | "i32.eqz" => SOME I32Eqz | "i32.eq" => SOME I32Eq | "i32.ne" => SOME I32Ne
    | "i32.lt_s" => SOME I32LtS | "i32.lt_u" => SOME I32LtU
    | "i32.gt_s" => SOME I32GtS | "i32.gt_u" => SOME I32GtU
    | "i32.le_s" => SOME I32LeS | "i32.le_u" => SOME I32LeU
    | "i32.ge_s" => SOME I32GeS | "i32.ge_u" => SOME I32GeU
    | "i64.add" => SOME I64Add | "i64.sub" => SOME I64Sub | "i64.mul" => SOME I64Mul
    | "i64.div_s" => SOME I64DivS | "i64.div_u" => SOME I64DivU
    | "i64.rem_s" => SOME I64RemS | "i64.rem_u" => SOME I64RemU
    | "i64.and" => SOME I64And | "i64.or" => SOME I64Or | "i64.xor" => SOME I64Xor
    | "i64.shl" => SOME I64Shl | "i64.shr_s" => SOME I64ShrS | "i64.shr_u" => SOME I64ShrU
    | "i64.rotl" => SOME I64Rotl | "i64.rotr" => SOME I64Rotr
    | "i64.eqz" => SOME I64Eqz | "i64.eq" => SOME I64Eq | "i64.ne" => SOME I64Ne
    | "i64.lt_s" => SOME I64LtS | "i64.lt_u" => SOME I64LtU
    | "i64.gt_s" => SOME I64GtS | "i64.gt_u" => SOME I64GtU
    | "i64.le_s" => SOME I64LeS | "i64.le_u" => SOME I64LeU
    | "i64.ge_s" => SOME I64GeS | "i64.ge_u" => SOME I64GeU
    | _ => NONE

  (* parse a linear instruction stream into an instr list *)
  fun parseBody (items, localOf, funcOf, globalOf) =
    let
      val toks = ref items
      fun adv () = case !toks of x :: xs => (toks := xs; x)
                              | [] => raise Wat "unexpected end of function body"
      fun peekTok () = case !toks of x :: _ => SOME x | [] => NONE
      fun atomArg () = case adv () of A a => a
                                    | _ => raise Wat "expected an immediate operand"

      fun resolve (byName, what) name =
        if String.isPrefix "$" name then
          (case byName name of SOME i => i | NONE => raise Wat ("unknown " ^ what ^ ": " ^ name))
        else (case Int.fromString name of SOME i => i
                                        | NONE => raise Wat ("bad " ^ what ^ " index"))
      val localRef  = resolve (localOf, "local")
      val funcRef   = resolve (funcOf, "function")
      val globalRef = resolve (globalOf, "global")
      fun labelRef (labels, name) =
        if String.isPrefix "$" name then
          (case labelIndex (labels, name) of SOME i => i
                                           | NONE => raise Wat ("unknown label: " ^ name))
        else (case Int.fromString name of SOME i => i | NONE => raise Wat "bad label index")

      fun readLabel () =
        case peekTok () of
          SOME (A a) => if String.isPrefix "$" a then (ignore (adv ()); SOME a) else NONE
        | _ => NONE
      fun readBlockType () =
        case peekTok () of
          SOME (L (A "result" :: rest)) =>
            (ignore (adv ());
             case rest of [] => BTEmpty | t :: _ => BTVal (parseValtype t))
        | SOME (L (A "param" :: _)) => (ignore (adv ()); BTEmpty)
        | _ => BTEmpty

      fun seq labels =
        let
          fun loop acc =
            case peekTok () of
              NONE => (List.rev acc, "")
            | SOME (A "end")  => (ignore (adv ()); (List.rev acc, "end"))
            | SOME (A "else") => (ignore (adv ()); (List.rev acc, "else"))
            | SOME _ => loop (one labels :: acc)
        in loop [] end
      and one labels =
        case adv () of
          A kw => instrOf (kw, labels)
        | L _ => raise Wat "folded instructions are not supported (use linear form)"
        | S _ => raise Wat "unexpected string literal in instruction position"
      and instrOf (kw, labels) =
        case kw of
          "block" =>
            let val l = readLabel () val bt = readBlockType ()
                val (b, term) = seq (l :: labels)
            in if term = "end" then Block (bt, b) else raise Wat "expected 'end' for block" end
        | "loop" =>
            let val l = readLabel () val bt = readBlockType ()
                val (b, term) = seq (l :: labels)
            in if term = "end" then Loop (bt, b) else raise Wat "expected 'end' for loop" end
        | "if" =>
            let
              val l = readLabel () val bt = readBlockType ()
              val (thn, term) = seq (l :: labels)
              val els =
                case term of
                  "else" => let val (e, t2) = seq (l :: labels)
                            in if t2 = "end" then e
                               else raise Wat "expected 'end' after else" end
                | "end" => []
                | _ => raise Wat "expected 'else' or 'end' for if"
            in If (bt, thn, els) end
        | "br"     => Br   (labelRef (labels, atomArg ()))
        | "br_if"  => BrIf (labelRef (labels, atomArg ()))
        | "return" => Return
        | "call"   => Call (funcRef (atomArg ()))
        | "drop"   => Drop
        | "select" => Select
        | "nop"    => Nop
        | "unreachable" => Unreachable
        | "local.get"  => LocalGet  (localRef  (atomArg ()))
        | "local.set"  => LocalSet  (localRef  (atomArg ()))
        | "local.tee"  => LocalTee  (localRef  (atomArg ()))
        | "global.get" => GlobalGet (globalRef (atomArg ()))
        | "global.set" => GlobalSet (globalRef (atomArg ()))
        | "i32.const"  => I32Const (toI32 (parseIntInf (atomArg ())))
        | "i64.const"  => I64Const (toI64 (parseIntInf (atomArg ())))
        | _ => (case simpleOp kw of
                  SOME i => i
                | NONE => raise Wat ("unknown instruction: " ^ kw))

      val (body, term) = seq []
      val () = case term of "" => ()
                          | _ => raise Wat ("unmatched '" ^ term ^ "'")
    in body end

  fun parseFunc (rest0, myIdx, funcOf, globalOf) =
    let
      val rest1 = case rest0 of
                    (A nm) :: r => if String.isPrefix "$" nm then r else rest0
                  | _ => rest0
      fun grabExports (items, exps) =
        case items of
          (L (A "export" :: [S name])) :: more =>
            grabExports (more, { name = name, kind = FuncExport, index = myIdx } :: exps)
        | _ => (items, List.rev exps)
      val (rest2, inlExps) = grabExports (rest1, [])
      val (params, results, locals, bodyItems) = gather (rest2, [], [], [])
      val localEnv = params @ locals
      fun localOf name =
        let fun go (_, []) = NONE
              | go (i, (n, _) :: xs) = if n = SOME name then SOME i else go (i + 1, xs)
        in go (0, localEnv) end
      val body = parseBody (bodyItems, localOf, funcOf, globalOf)
      val ft  = { params = List.map #2 params, results = results }
      val fdef = { typeIdx = myIdx, locals = List.map #2 locals, body = body }
    in (ft, fdef, inlExps) end

  fun parseGlobal (rest0, gi, globalOf) =
    let
      val rest1 = case rest0 of
                    (A nm) :: r => if String.isPrefix "$" nm then r else rest0
                  | _ => rest0
      fun grabExports (items, exps) =
        case items of
          (L (A "export" :: [S name])) :: more =>
            grabExports (more, { name = name, kind = GlobalExport, index = gi } :: exps)
        | _ => (items, List.rev exps)
      val (rest2, inlExps) = grabExports (rest1, [])
      val (typ, mut, rest3) =
        case rest2 of
          (L [A "mut", t]) :: r => (parseValtype t, Var, r)
        | t :: r => (parseValtype t, Const, r)
        | [] => raise Wat "global is missing a type"
      fun gref g = if String.isPrefix "$" g
                   then (case globalOf g of SOME i => i | NONE => raise Wat ("unknown global: " ^ g))
                   else valOf (Int.fromString g)
      val init =
        case rest3 of
          [L (A "i32.const" :: [A k])] => [I32Const (toI32 (parseIntInf k))]
        | [L (A "i64.const" :: [A k])] => [I64Const (toI64 (parseIntInf k))]
        | [L (A "global.get" :: [A g])] => [GlobalGet (gref g)]
        | _ => raise Wat "unsupported global initializer (expected a constant)"
    in ({ typ = typ, mut = mut, init = init }, inlExps) end

  fun parse src =
    let
      val fields =
        case parseSexps (tokenize src) of
          (L (A "module" :: fs)) :: _ => fs
        | _ => raise Wat "expected a top-level (module ...)"

      fun nameOf rest =
        case rest of (A a) :: _ => if String.isPrefix "$" a then SOME a else NONE
                   | _ => NONE

      fun scan (items, fi, gi, fnames, gnames) =
        case items of
          [] => (List.rev fnames, List.rev gnames)
        | (L (A "func" :: rest)) :: more =>
            scan (more, fi + 1, gi, (fi, nameOf rest) :: fnames, gnames)
        | (L (A "global" :: rest)) :: more =>
            scan (more, fi, gi + 1, fnames, (gi, nameOf rest) :: gnames)
        | (L (A "import" :: _)) :: _ =>
            raise Wat "imports are not supported by the text parser"
        | _ :: more => scan (more, fi, gi, fnames, gnames)

      val (fnames, gnames) = scan (fields, 0, 0, [], [])
      fun funcOf name   = Option.map #1 (List.find (fn (_, n) => n = SOME name) fnames)
      fun globalOf name = Option.map #1 (List.find (fn (_, n) => n = SOME name) gnames)

      fun refTop (byName, what) r =
        if String.isPrefix "$" r
        then (case byName r of SOME i => i | NONE => raise Wat ("unknown " ^ what ^ ": " ^ r))
        else (case Int.fromString r of SOME i => i | NONE => raise Wat ("bad " ^ what ^ " index"))
      val funcTop = refTop (funcOf, "function")
      val globalTop = refTop (globalOf, "global")

      fun exportTarget (name, L [A "func", A r]) =
            { name = name, kind = FuncExport, index = funcTop r }
        | exportTarget (name, L [A "global", A r]) =
            { name = name, kind = GlobalExport, index = globalTop r }
        | exportTarget _ = raise Wat "unsupported export target"

      fun build (items, fi, gi, types, funcs, globs, exps, start) =
        case items of
          [] => { types = List.rev types, funcs = List.rev funcs
                , globals = List.rev globs, exports = List.rev exps
                , start = start, numImportedFuncs = 0 }
        | (L (A "func" :: rest)) :: more =>
            let val (ft, fdef, inl) = parseFunc (rest, fi, funcOf, globalOf)
            in build (more, fi + 1, gi, ft :: types, fdef :: funcs, globs,
                      List.revAppend (inl, exps), start) end
        | (L (A "global" :: rest)) :: more =>
            let val (g, inl) = parseGlobal (rest, gi, globalOf)
            in build (more, fi, gi + 1, types, funcs, g :: globs,
                      List.revAppend (inl, exps), start) end
        | (L (A "export" :: [S name, target])) :: more =>
            build (more, fi, gi, types, funcs, globs, exportTarget (name, target) :: exps, start)
        | (L (A "start" :: [A r])) :: more =>
            build (more, fi, gi, types, funcs, globs, exps, SOME (funcTop r))
        | _ :: more =>   (* (type ...), (memory ...), (table ...), comments handled, etc. *)
            build (more, fi, gi, types, funcs, globs, exps, start)
    in
      build (fields, 0, 0, [], [], [], [], NONE)
    end
end
