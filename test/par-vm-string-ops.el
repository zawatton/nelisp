;;; par-vm-string-ops.el --- GNU VM string comparisons -*- lexical-binding: t; -*-

;; Two required arguments are already on the VM operand stack.
(defconst par-vm-string-equal
  (make-byte-code 514 (unibyte-string 152 135) [] 2))
(defconst par-vm-string-less
  (make-byte-code 514 (unibyte-string 153 135) [] 2))
(defconst par-vm-string-length
  (make-byte-code 257 (unibyte-string 71 135) [] 1))

(dolist (pair (list '("" "") '("a" "ab") '("ab" "a") '("ab" "ac")
                   '("a\0b" "a\0c") '("猫" "犬") '("猫" "猫")
                   (list (unibyte-string 255) (unibyte-string 254))
                   (list (propertize "cat" 'face 'bold) "cat")
                   '(cat "cat") '(nil "nil") '(3 "cat") '("cat" [cat])))
  (prin1 (list 'comparisons
               (condition-case err (apply par-vm-string-equal pair) (error err))
               (condition-case err (apply par-vm-string-less pair) (error err))))
  (terpri))

(prin1 (list 'lengths (mapcar par-vm-string-length
                            (list "" "猫" "a猫" "a\0b" (unibyte-string 255) [1 2]))))
(terpri)
