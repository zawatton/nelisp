;;; generate-native-bytecode-inventory.el --- Freeze GNU bytecode names -*- lexical-binding: t; -*-

(require 'bytecomp)
(require 'json)
(require 'cl-lib)

(defun nelisp-native-bytecode-inventory--sha256 (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defconst nelisp-native-bytecode-inventory--bytecomp-hash
  "37c0ad666e0f2e8aadd9559b659ab90710e88ef33c44655eb1742fbeb5819cc6")
(defconst nelisp-native-bytecode-inventory--comp-hash
  "d55f2f0f75ef4f6095119272f8830806648e3dc9d9fe916d329aadd71e78f867")

(let* ((directory (file-name-directory (locate-library "bytecomp")))
       (expected-bytecomp nelisp-native-bytecode-inventory--bytecomp-hash)
       (expected-comp nelisp-native-bytecode-inventory--comp-hash))
  (unless (and (equal emacs-version "31.1")
               (equal (nelisp-native-bytecode-inventory--sha256
                       (expand-file-name "bytecomp.el.gz" directory))
                      expected-bytecomp)
               (equal (nelisp-native-bytecode-inventory--sha256
                       (expand-file-name "comp.el.gz" directory))
                      expected-comp))
    (error "Installed GNU bytecode sources do not match the pinned dialect")))

(let* ((opcodes (vconcat (mapcar (lambda (name)
                                  (and name (symbol-name name)))
                                (append byte-code-vector nil))))
       (stack-adjust (vconcat (append byte-stack+-info nil)))
       (inventory `((schema . 1)
                    (dialect . "GNU Emacs 31.1")
                    (source . ((bytecomp.el.gz . ,nelisp-native-bytecode-inventory--bytecomp-hash)
                               (comp.el.gz . ,nelisp-native-bytecode-inventory--comp-hash)))
                    (opcode-count . 256)
                    (named-opcode-count . ,(cl-count-if #'identity byte-code-vector))
                    (opcodes . ,opcodes)
                    (stack-adjust . ,stack-adjust))))
  (with-temp-file "test/fixtures/native-bytecode/gnu-31.1-opcodes.json"
    (insert (json-encode inventory) "\n")))

;;; generate-native-bytecode-inventory.el ends here
