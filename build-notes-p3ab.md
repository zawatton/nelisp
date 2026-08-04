# P3a / P3b integrator recipe

All programs below are self-contained and seed every byte they read.

## P3a allocator / globals

Source form:

```elisp
(seq
  (defun boot ()
    (let ((p (alloc-bytes 131072 8)))
      (seq
       (ptr-write-u64 p 0 305419896)
       (ptr-read-u64 p 0)))))
```

Compile:

```powershell
$env:HOME=(Get-Location).Path
$env:USERPROFILE=(Get-Location).Path
emacs --batch -Q -L lisp `
  --eval "(setq user-emacs-directory (expand-file-name \".emacs.d/\" default-directory))" `
  --eval "(progn
            (load \"nelisp-aot-compiler\")
            (nelisp-aot-compile-to-object
             '(seq
               (defun boot ()
                 (let ((p (alloc-bytes 131072 8)))
                   (seq
                    (ptr-write-u64 p 0 305419896)
                    (ptr-read-u64 p 0)))))
             \"p3a-boot.wasm\" :arch 'wasm32 :format 'wasm))"
```

Driver:

```powershell
node tools/wasm-driver.mjs p3a-boot.wasm boot 305419896
node -e "const fs=require('fs');(async()=>{const bytes=fs.readFileSync('p3a-boot.wasm');const {instance}=await WebAssembly.instantiate(bytes,{});console.log(instance.exports.heap_ptr.value);})();"
```

Expected:

- `boot` returns `305419896`
- `heap_ptr` is greater than `65536`
- the large `131072`-byte alloc exercises the grow loop

## P3b baked data boot

Source form:

```elisp
(seq
  (data-blob boot-form
             (1 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0)
             rodata
             ((8 boot-args 0)))
  (data-blob boot-args
             (1 0 0 0 0 0 0 0
              2 0 0 0 0 0 0 0)
             rodata)
  (defun _start ()
    (if (= (ptr-read-u32 (data-addr boot-form) 0) 1)
        (let ((aptr (ptr-read-u64 (data-addr boot-form) 8))
              (cell (alloc-bytes 8 8)))
          (seq
           (ptr-write-u64 cell 0
                          (+ (ptr-read-u64 aptr 0)
                             (ptr-read-u64 aptr 8)))
           (ptr-read-u64 cell 0)))
      -1)))
```

Compile:

```powershell
$env:HOME=(Get-Location).Path
$env:USERPROFILE=(Get-Location).Path
emacs --batch -Q -L lisp `
  --eval "(setq user-emacs-directory (expand-file-name \".emacs.d/\" default-directory))" `
  --eval "(progn
            (load \"nelisp-aot-compiler\")
            (nelisp-aot-compile-to-object
             '(seq
               (data-blob boot-form
                          (1 0 0 0 0 0 0 0
                           0 0 0 0 0 0 0 0)
                          rodata
                          ((8 boot-args 0)))
               (data-blob boot-args
                          (1 0 0 0 0 0 0 0
                           2 0 0 0 0 0 0 0)
                          rodata)
               (defun _start ()
                 (if (= (ptr-read-u32 (data-addr boot-form) 0) 1)
                     (let ((aptr (ptr-read-u64 (data-addr boot-form) 8))
                           (cell (alloc-bytes 8 8)))
                       (seq
                        (ptr-write-u64 cell 0
                                       (+ (ptr-read-u64 aptr 0)
                                          (ptr-read-u64 aptr 8)))
                        (ptr-read-u64 cell 0)))
                   -1)))
             \"p3b-boot.wasm\" :arch 'wasm32 :format 'wasm))"
```

Driver:

```powershell
node tools/wasm-driver.mjs p3b-boot.wasm _start 3
node -e "const fs=require('fs');(async()=>{const bytes=fs.readFileSync('p3b-boot.wasm');const {instance}=await WebAssembly.instantiate(bytes,{});console.log(instance.exports.heap_ptr.value);})();"
```

Expected:

- `_start` returns `3`
- `heap_ptr` advances from its initial value by `8`
- the baked pointer at `boot-form + 8` is patched at emit time to the absolute linear-memory address of `boot-args`

## P3c handoff

- `lisp/nelisp-artifact.el` is untouched in this task.
- `_start` export support here is the normal wasm function export path; the P3c runtime-image lane can build on that without changing the P3a/P3b encoder or writer work.
