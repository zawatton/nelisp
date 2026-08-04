# P3c integrator recipe

All programs below are self-contained and seed every byte they read.

## Runtime-image wasm lane

Runtime image:

```elisp
;;; nelisp-runtime-image source-v1
(progn
  (data-blob nelisp-boot-form
             (1 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0)
             rodata
             ((8 nelisp-boot-args 0)))
  (data-blob nelisp-boot-args
             (1 0 0 0 0 0 0 0
              2 0 0 0 0 0 0 0)
             rodata)
  (defun nelisp-boot-eval (form)
    (if (= (ptr-read-u32 form 0) 1)
        (let ((aptr (ptr-read-u64 form 8))
              (cell (alloc-bytes 8 8)))
          (seq
           (ptr-write-u64 cell 0
                          (+ (ptr-read-u64 aptr 0)
                             (ptr-read-u64 aptr 8)))
           (ptr-read-u64 cell 0)))
      -1))
  (defun _start ()
    (nelisp-boot-eval (data-addr nelisp-boot-form))))
```

Compile via the new lane:

```powershell
$env:HOME=(Get-Location).Path
$env:XDG_CONFIG_HOME=(Get-Location).Path
emacs --batch -Q -L lisp -L src `
  --eval '(setq load-prefer-newer t)' `
  --eval "(progn
            (require 'nelisp-artifact)
            (compile-runtime-image
             '(\"compile-runtime-image\" \"--kind\" \"auto\"
               \"--target\" \"wasm32-wasi\"
               \"--input\" \"tools/wasm-runtime-image-p3c.nlri\"
               \"--output\" \"runtime-image.wasm\")))"
```

Smoke:

```powershell
node tools/wasm-driver.mjs runtime-image.wasm _start 3
node -e "const fs=require('fs');(async()=>{const bytes=fs.readFileSync('runtime-image.wasm');const {instance}=await WebAssembly.instantiate(bytes,{});const before=instance.exports.heap_ptr.value;const result=instance.exports._start();const after=instance.exports.heap_ptr.value;console.log('result='+result.toString());console.log('heap_ptr_before='+before);console.log('heap_ptr_after='+after);})();"
```

Expected:

- `_start` returns `3`
- `heap_ptr_before` is the baked heap base (`4128` with the current 32-byte node pool and 8-byte alignment)
- `heap_ptr_after` is `4136`
- the baked form is an AST in Data segments, not a string parse
- `nelisp-boot-form + 8` is relocated to the absolute linear-memory address of `nelisp-boot-args`

## Make target

```powershell
make wasm-runtime-image-smoke
```

Expected:

- `target/wasm-runtime-image/runtime-image.wasm` is produced
- `node tools/wasm-driver.mjs target/wasm-runtime-image/runtime-image.wasm _start 3` passes

## Regression commands

P3a / P3b:

```powershell
node tools/wasm-proofs/p3-05-bump-alloc.mjs
node tools/wasm-proofs/p3-06-baked-boot.mjs
node tools/wasm-proofs/p3-run-all.mjs
```

P1 / P2:

```powershell
make wasm-smoke
node tools/wasm-proofs/run-all.mjs
```

Expected:

- `p3-05` keeps the allocator contract green
- `p3-06` keeps the relocated baked-boot capstone green
- `p3-run-all.mjs` prints `ALL P3 PROOFS OK`
- `make wasm-smoke` keeps the P1 arithmetic / locals smoke green
- `run-all.mjs` keeps the P2 proof pack green
