;;; nelisp-cc-rootstack-token-test.el --- Root pin ticket ABI tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'nelisp-cc-rootstack)
(require 'nelisp-native-load)
(require 'nelisp-standalone-build)

(defun nelisp-cc-rootstack-token-test--form (name)
  (seq-find (lambda (form)
              (and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) name)))
            (cdr nelisp-cc-rootstack--source)))

(defun nelisp-cc-rootstack-token-test--contains (tree item)
  (if (consp tree)
      (or (equal tree item)
          (nelisp-cc-rootstack-token-test--contains (car tree) item)
          (nelisp-cc-rootstack-token-test--contains (cdr tree) item))
    (equal tree item)))

(ert-deftest nelisp-cc-rootstack-v2-ticket-api-is-registered-and-separated ()
  (dolist (name '(nl_root_pin_begin_v2 nl_root_pin_reserve_v2
                  nl_root_pin_end_v2 nl_root_pin_v2_next_token
                  nl_root_pin_v2_commit_token nl_root_pin_slot_v2))
    (should (nelisp-cc-rootstack-token-test--form name)))
  (dolist (name '("nl_root_pin_begin_v2" "nl_root_pin_reserve_v2"
                  "nl_root_pin_end_v2"))
    (should (member name nelisp-native-load-bridgeable-symbols))
    (should (member name nelisp-standalone--reader-neln-bridgeable-symbols)))
  (should (member "nl_root_pin_slot_v2"
                  nelisp-native-load-bridgeable-symbols))
  (should (member "nl_root_pin_slot_v2"
                  nelisp-standalone--reader-neln-bridgeable-symbols))
  (should (>= (nelisp-standalone--root-pin-region-offset)
              (+ (nelisp-standalone--driver-bss-base-size) 64)))
  (let ((legacy-begin
         (nelisp-cc-rootstack-token-test--form 'nl_root_pin_begin))
        (legacy-reserve
         (nelisp-cc-rootstack-token-test--form 'nl_root_pin_reserve))
        (legacy-end
         (nelisp-cc-rootstack-token-test--form 'nl_root_pin_end))
        (v2-begin
         (nelisp-cc-rootstack-token-test--form 'nl_root_pin_begin_v2)))
    (should (nelisp-cc-rootstack-token-test--contains legacy-begin 48))
    (should (nelisp-cc-rootstack-token-test--contains legacy-reserve 48))
    (should (nelisp-cc-rootstack-token-test--contains legacy-end 48))
    (should (nelisp-cc-rootstack-token-test--contains
             (nelisp-cc-rootstack-token-test--form
              'nl_root_pin_v2_next_token) 40))
    (should (nelisp-cc-rootstack-token-test--contains
             (nelisp-cc-rootstack-token-test--form
              'nl_root_pin_v2_next_token) 9223372036854775807))
    (should (nelisp-cc-rootstack-token-test--contains v2-begin 32))))

(provide 'nelisp-cc-rootstack-token-test)

;;; nelisp-cc-rootstack-token-test.el ends here
