P2 wasm backend integration notes
=================================

Compile
-------

Use:

```elisp
(nelisp-aot-compile-to-object
 '(defun hello-clock ()
    (wasi-call clock_time_get 0 0 1040)
    (ptr-read-u64 1040 0))
 :arch 'wasm32 :format 'wasm)

;; hello-file must seed linear memory itself: path "out.txt" at 1048,
;; payload "hello\n" at 1024, iov {buf=1024,len=6} at 1040; fd out-param
;; at 1032, fd_write nwritten out-param at 1056. (Verified 2026-07-12.)
(nelisp-aot-compile-to-object
 '(defun hello-file ()
    (seq
     (ptr-write-u8 1048 0 111) (ptr-write-u8 1048 1 117)
     (ptr-write-u8 1048 2 116) (ptr-write-u8 1048 3 46)
     (ptr-write-u8 1048 4 116) (ptr-write-u8 1048 5 120)
     (ptr-write-u8 1048 6 116)
     (ptr-write-u8 1024 0 104) (ptr-write-u8 1024 1 101)
     (ptr-write-u8 1024 2 108) (ptr-write-u8 1024 3 108)
     (ptr-write-u8 1024 4 111) (ptr-write-u8 1024 5 10)
     (ptr-write-u32 1040 0 1024) (ptr-write-u32 1040 4 6)
     (wasi-call path_open 3 0 1048 7 9 70 0 0 1032)
     (wasi-call fd_write (ptr-read-u32 1032 0) 1040 1 1056)
     (wasi-call fd_close (ptr-read-u32 1032 0))
     0))
 :arch 'wasm32 :format 'wasm)

(nelisp-aot-compile-to-object
 '(defun cat () (catch 11 (catch 22 (throw 22 42))))
 :arch 'wasm32 :format 'wasm)

(nelisp-aot-compile-to-object
 '(defun cot () (catch 11 (catch 22 (throw 11 99))))
 :arch 'wasm32 :format 'wasm)

(nelisp-aot-compile-to-object
 '(defun up-normal ()
    (let ((n 0))
      (unwind-protect 7 (setq n (+ n 1)))))
 :arch 'wasm32 :format 'wasm)

(nelisp-aot-compile-to-object
 '(defun up-throw ()
    (catch 1
      (let ((n 0))
        (unwind-protect (throw 1 5) (setq n (+ n 1))))))
 :arch 'wasm32 :format 'wasm)

(nelisp-aot-compile-to-object
 '(defun addx (a b) (+ (extern-call ext_add a b) 1000))
 :arch 'wasm32 :format 'wasm)
```

Run
---

Use:

```powershell
node tools/wasm-wasi-driver.mjs hello-clock.wasm wasi-reactor hello-clock 1 --stub-clock 123456789
node tools/wasm-wasi-driver.mjs hello-file.wasm wasi-reactor hello-file 0 --preopen /sandbox=$env:TEMP
# MSYS bash mangles "/sandbox=..." via path conversion -> prefix with
# MSYS2_ARG_CONV_EXCL="*" when invoking from an MSYS shell.
node tools/wasm-wasi-driver.mjs cat.wasm plain cat 42
node tools/wasm-wasi-driver.mjs cot.wasm plain cot 99
node tools/wasm-wasi-driver.mjs up-normal.wasm plain up-normal 7
node tools/wasm-wasi-driver.mjs up-throw.wasm plain up-throw 5
node tools/wasm-wasi-driver.mjs addx.wasm plain addx 1007 --arg 3 --arg 4
```

Expected values
---------------

- `hello-file` returns `0` and writes `hello\n` to `/sandbox/out.txt`.
- `hello-clock` returns a nonzero timestamp; with `--stub-clock 123456789` the result is deterministic.
- `cat` returns `42`.
- `cot` returns `99`.
- `up-normal` returns `7`.
- `up-throw` returns `5`.
- `addx` returns `1007`.

P1 regression list
------------------

- `42`
- `fact`
- `fib`
- `while`
- `funcall`
