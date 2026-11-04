;;; wasm-intern-symbol-oracle.el --- Clean native hash oracle -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'nelisp-aot-compiler)
(require 'nelisp-cc-nlstr-direct-ops)
(defvar nelisp-standalone--target)
(defun wasm-intern-symbol-oracle ()
  "Select actual providers, compile and execute the hash plus genuine mask."
  (load (expand-file-name "scripts/nelisp-standalone-build.el") nil t)
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (arena (nelisp-standalone--target-arena-source))
         (pool (cdr nelisp-cc-nlstr-direct-ops--alloc-str-source))
         (init (cl-find 'nl_intern_region_init (cdr arena) :key #'cadr))
         (pending '(nl_alloc_symbol)) seen definitions
         (directory "target/wasm-intern-symbol/")
         (primitives '(+ - * = < > <= >= logand logxor ptr-read-u8 ptr-read-u64
                        ptr-write-u8 ptr-write-u64 alloc-bytes)))
    ;; Require only the reviewed syntax walker, not the composition driver.
    (load (expand-file-name "tools/ai/wasm-target-alloc.el") nil t)
    (require 'wasm-intern-symbol)
    (while pending
      (let ((name (pop pending)))
        (unless (memq name seen)
          (push name seen)
          (let ((definition (cl-find name pool :key #'cadr)))
            (unless definition (error "Unresolved intern callee %s" name))
            (push definition definitions)
            (dolist (callee (wasm-target-alloc-calls (nthcdr 3 definition)))
              (unless (memq callee primitives) (push callee pending)))))))
    (setq definitions (sort definitions (lambda (a b) (string< (symbol-name (cadr a)) (symbol-name (cadr b))))))
    (wasm-intern-symbol-region-contract init definitions)
    (let* ((hash (cl-find 'nl_intern_hash definitions :key #'cadr))
           (pos (cl-find 'nl_alloc_symbol_pos definitions :key #'cadr))
           (mask-forms (wasm-rootstack--forms pos
                         (lambda (form) (and (eq (car form) 'logand)
                                            (eq (car-safe (cadr form)) 'nl_intern_hash)))))
           (mask (nth 2 (car mask-forms)))
           (hash-call (cadr (car mask-forms)))
           (seed (nth 4 hash-call))
           (object (concat directory "native.o"))
           (driver (concat directory "native.c"))
           (binary (concat directory "native-oracle"))
           (output (concat directory "native-results.json")))
      (unless (and (= (length definitions) 18) (= (length mask-forms) 1)
                   (equal hash-call `(nl_intern_hash bytes-ptr 0 n ,seed))
                   (integerp mask) (= mask 1048575) (integerp seed))
        (error "Native intern oracle syntax mismatch"))
      (nelisp-aot-compile-to-object
       `(seq ,hash ,init (defun intern_oracle_slot (bytes-ptr n) (logand ,hash-call ,mask))) object)
      (with-temp-file driver
        (insert "#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <sys/mman.h>\n"
                "extern uint64_t intern_oracle_slot(uint64_t,uint64_t),nl_intern_region_init(void);\n"
                "static uint64_t slot(const char *s){return intern_oracle_slot((uint64_t)(uintptr_t)s,strlen(s));}\n"
                "int main(void){const char *names[]={\"\",\"x\",\"abc\",\"long-name-tail\",\"nil\",\"t\",\"nilx\",\"tx\",\"xil\",\"nxl\",\"nix\",\"u\",\"ni\"};\n"
                ;; The real init writes its genuine absolute metadata atoms.
                ;; Reserve that page with NOREPLACE, never overwrite a mapping.
                (format "void *meta=mmap((void*)(uintptr_t)%d,4096,3,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED_NOREPLACE,-1,0);if(meta==MAP_FAILED)return 3;\n"
                        (nelisp-standalone-arena-rewrite-base))
                "uint64_t ret=nl_intern_region_init();\n"
                (format "uint64_t base=*(uint64_t*)(uintptr_t)%d,cursor=*(uint64_t*)(uintptr_t)%d;\n"
                        (nth 1 (nth 1 (nth 3 (nth 2 (nth 3 init)))))
                        (nth 1 (nth 2 (nth 3 (nth 2 (nth 3 init))))))
                "if(base<4096||cursor!=base+16777216)return 4;\n"
                "printf(\"{\\\"init_return\\\":%llu,\\\"cases\\\":[\",(unsigned long long)ret);for(size_t i=0;i<sizeof(names)/sizeof(names[0]);i++)printf(\"%s[\\\"%s\\\",%zu,%llu]\",i?\",\":\"\",names[i],strlen(names[i]),(unsigned long long)slot(names[i]));\n"
                "int *seen=malloc(1048576*sizeof(int));if(!seen)return 2;for(int i=0;i<1048576;i++)seen[i]=-1;\n"
                "char s[64],a[64];int found=0;for(int i=0;i<20000;i++){snprintf(s,sizeof(s),\"collision-%d\",i);uint64_t k=slot(s);if(seen[k]>=0){snprintf(a,sizeof(a),\"collision-%d\",seen[k]);if(strlen(a)!=strlen(s))continue;printf(\",[\\\"%s\\\",%zu,%llu],[\\\"%s\\\",%zu,%llu]\",a,strlen(a),(unsigned long long)slot(a),s,strlen(s),(unsigned long long)k);found=1;break;}seen[k]=i;}printf(\"],\\\"collision\\\":%s,\\\"prefix_cases\\\":[\",found?\"true\":\"false\");free(seen);\n"
                ;; Search names beginning with x, with five base-26 suffix bytes.
                "int prefix=0;for(int i=0;i<11881376;i++){int v=i;s[0]='x';for(int j=5;j>=1;j--){s[j]='a'+v%26;v/=26;}s[6]=0;if(slot(s)==slot(\"x\")){printf(\"[\\\"%s\\\",6,%llu],[\\\"x\\\",1,%llu]\",s,(unsigned long long)slot(s),(unsigned long long)slot(\"x\"));prefix=1;break;}}printf(\"]}\\n\");munmap((void*)(uintptr_t)base,67108864);munmap(meta,4096);return found&&prefix?0:5;}\n"))
      (with-temp-buffer
        (unless (equal 0 (call-process "cc" nil t nil "-no-pie" driver object "-o" binary))
          (error "Native hash link: %s" (buffer-string))))
      (with-temp-buffer
        (let ((rc (call-process (expand-file-name binary) nil t nil)))
          (with-temp-file (concat directory "native.rc") (insert (format "%s\n" rc)))
          (unless (equal rc 0) (error "Native hash oracle rc=%s" rc)))
        (write-region (point-min) (point-max) output nil 'silent))
      (let* ((files (sort (delete-dups (delq nil (mapcar #'car load-history))) #'string<))
             (print-length nil) (print-level nil) (print-circle nil))
        (when (cl-some (lambda (file) (string-suffix-p ".eln" file)) files)
          (error "Native oracle loaded eln"))
        (princ "WASM-INTERN-SYMBOL=")
        (prin1 (list :definitions definitions :init init :mask mask :seed seed
                     :native-files (mapcar (lambda (file) (cons file (wasm-rootstack--hash file)))
                                           (list object driver binary output (concat directory "native.rc")))
                     :files (mapcar (lambda (file) (cons file (wasm-rootstack--hash file))) files)))
        (terpri)))))
(provide 'wasm-intern-symbol-oracle)
