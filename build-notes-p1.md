# wasm P1 integrator recipe

I did not run these heavy compiler commands on this Windows host. Use them as the post-merge verification recipe.

## Lightweight ERT only

Run the wasm encoder tests without loading the large AOT compiler body through an end-to-end compile:

```powershell
emacs --batch -Q -L lisp -L test -l test/nelisp-asm-wasm-test.el -f ert-run-tests-batch-and-exit
```

## End-to-end wasm builds for the integrator

Each compile command below is intentionally left for the integrator because it loads `lisp/nelisp-aot-compiler.el`.

### 1. Recursive factorial

Source:

```elisp
'(defun fact (n)
   (if (<= n 1)
       1
     (* n (fact (- n 1)))))
```

Compile:

```powershell
emacs --batch -Q -L lisp --eval "(progn (require 'nelisp-aot-compiler) (nelisp-aot-compile-to-object '(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))) \"fact-p1.wasm\" :arch 'wasm32 :format 'wasm))"
```

Run:

```powershell
node tools/wasm-driver.mjs fact-p1.wasm fact 120 5
```

### 2. Recursive Fibonacci

Source:

```elisp
'(defun fib (n)
   (if (<= n 1)
       n
     (+ (fib (- n 1))
        (fib (- n 2)))))
```

Compile:

```powershell
emacs --batch -Q -L lisp --eval "(progn (require 'nelisp-aot-compiler) (nelisp-aot-compile-to-object '(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) \"fib-p1.wasm\" :arch 'wasm32 :format 'wasm))"
```

Run:

```powershell
node tools/wasm-driver.mjs fib-p1.wasm fib 55 10
```

### 3. While accumulator

Source:

```elisp
'(defun sum-to (n)
   (let ((i 0)
         (acc 0))
     (seq
      (while (< i n)
        (setq acc (+ acc i)
              i (+ i 1)))
      acc)))
```

Compile:

```powershell
emacs --batch -Q -L lisp --eval "(progn (require 'nelisp-aot-compiler) (nelisp-aot-compile-to-object '(defun sum-to (n) (let ((i 0) (acc 0)) (seq (while (< i n) (setq acc (+ acc i) i (+ i 1))) acc))) \"while-p1.wasm\" :arch 'wasm32 :format 'wasm))"
```

Run:

```powershell
node tools/wasm-driver.mjs while-p1.wasm sum-to 45 10
```

### 4. Indirect function dispatch

This backend work emits `call_indirect` for the function-value surface represented by `call-ptr` plus `addr-of`. That is the wasm primitive to verify here.

Source:

```elisp
'(seq
  (defun inc (x) (+ x 1))
  (defun dispatch (x)
    (let ((f (addr-of inc)))
      (call-ptr f x))))
```

Compile:

```powershell
emacs --batch -Q -L lisp --eval "(progn (require 'nelisp-aot-compiler) (nelisp-aot-compile-to-object '(seq (defun inc (x) (+ x 1)) (defun dispatch (x) (let ((f (addr-of inc))) (call-ptr f x)))) \"indirect-p1.wasm\" :arch 'wasm32 :format 'wasm))"
```

Run:

```powershell
node tools/wasm-driver.mjs indirect-p1.wasm dispatch 42 41
```

Suggested spot-check:

```powershell
node -e "const fs=require('fs');const bytes=fs.readFileSync('indirect-p1.wasm');console.log(bytes.includes(Buffer.from([0x11])));"
```

Expected: `true` because opcode `0x11` is `call_indirect`.

### 5. Float sqrt parity guard

The current wasm P1 path supports the doc-approved narrow form `sqrt` lowered to native `f64.sqrt`, bridged through raw bits.

Source:

```elisp
'(defun sqrt-bits ()
   (f64-bits (f64-call sqrt (bits-to-f64 4621256167635550208))))
```

`4621256167635550208` is the IEEE-754 bit pattern for `9.0`. The expected return is the bit pattern for `3.0`, `4613937818241073152`.

Compile:

```powershell
emacs --batch -Q -L lisp --eval "(progn (require 'nelisp-aot-compiler) (nelisp-aot-compile-to-object '(defun sqrt-bits () (f64-bits (f64-call sqrt (bits-to-f64 4621256167635550208)))) \"sqrt-p1.wasm\" :arch 'wasm32 :format 'wasm))"
```

Run:

```powershell
node tools/wasm-driver.mjs sqrt-p1.wasm sqrt-bits 4613937818241073152
```
