(module
  ;; add : (i32, i32) -> i32
  (func $add (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add)

  ;; iterative fibonacci : fib(n) using a loop
  (func $fib (param $n i32) (result i32)
    (local $a i32) (local $b i32) (local $i i32)
    i32.const 0
    local.set $a
    i32.const 1
    local.set $b
    i32.const 0
    local.set $i
    block $done
      loop $cont
        local.get $i
        local.get $n
        i32.ge_s
        br_if $done
        local.get $a
        local.get $b
        i32.add
        local.get $b
        local.set $a
        local.set $b
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $cont
      end
    end
    local.get $a)

  ;; iterative factorial : fact(n)
  (func $fact (param $n i32) (result i32)
    (local $acc i32) (local $i i32)
    i32.const 1
    local.set $acc
    i32.const 1
    local.set $i
    block $done
      loop $cont
        local.get $i
        local.get $n
        i32.gt_s
        br_if $done
        local.get $acc
        local.get $i
        i32.mul
        local.set $acc
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br $cont
      end
    end
    local.get $acc)

  ;; recursive fibonacci : exercises call / if-else / recursion
  (func $fibrec (param $n i32) (result i32)
    local.get $n
    i32.const 2
    i32.lt_s
    if (result i32)
      local.get $n
    else
      local.get $n
      i32.const 1
      i32.sub
      call $fibrec
      local.get $n
      i32.const 2
      i32.sub
      call $fibrec
      i32.add
    end)

  ;; 64-bit multiply : exercises i64 (needs >32-bit magnitudes)
  (func $mul64 (param $a i64) (param $b i64) (result i64)
    local.get $a
    local.get $b
    i64.mul)

  (export "add"    (func $add))
  (export "fib"    (func $fib))
  (export "fact"   (func $fact))
  (export "fibrec" (func $fibrec))
  (export "mul64"  (func $mul64)))
