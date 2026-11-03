;;; ja-dic-utl-test.el --- tests for lightweight Japanese dictionary helpers  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(load "ja-dic-utl" nil t)

(ert-deftest ja-dic-utl-test/feature-and-okurigana-table ()
  (should (featurep 'ja-dic-utl))
  (should (eq (cdr (assq #x304b skkdic-okurigana-table)) ?k))
  (should (eq (cdr (assq #x3093 skkdic-okurigana-table)) ?n)))

(ert-deftest ja-dic-utl-test/merge-head-and-tail ()
  (should (equal (skkdic-merge-head-and-tail '("接" "x") '("尾" "y") t)
                 '("接尾" "接y" "x尾" "xy")))
  (should (equal (skkdic-merge-head-and-tail '("接" "接頭") '("尾" "語尾") nil)
                 '("接頭語尾"))))

(ert-deftest ja-dic-utl-test/lookup-okuri-nasi-from-hash ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "かな" '("仮名" "かな") table)
    (let ((skkdic-okuri-nasi table)
          (skkdic-okuri-ari nil)
          (skkdic-prefix nil)
          (skkdic-postfix nil))
      (should (equal (skkdic-lookup-key "かな" 2)
                     '("仮名" "かな"))))))

(ert-deftest ja-dic-utl-test/lookup-prefix-postfix-and-okuri ()
  (let ((base (make-hash-table :test 'equal))
        (prefix (make-hash-table :test 'equal))
        (postfix (make-hash-table :test 'equal))
        (okuri (make-hash-table :test 'equal)))
    (puthash "ほん" '("本語") base)
    (puthash "お" '("御礼") prefix)
    (puthash "さん" '("様") postfix)
    (puthash "かk" '("書") okuri)
    (let ((skkdic-okuri-nasi base)
          (skkdic-prefix prefix)
          (skkdic-postfix postfix)
          (skkdic-okuri-ari okuri))
      (should (member "御礼本語" (skkdic-lookup-key "おほん" 3)))
      (should (member "本語様" (skkdic-lookup-key "ほんさん" 4 t)))
      (should (equal (skkdic-lookup-key "かか" 2)
                     '("書か"))))))

(provide 'ja-dic-utl-test)

;;; ja-dic-utl-test.el ends here
