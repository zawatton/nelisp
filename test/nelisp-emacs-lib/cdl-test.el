;;; cdl-test.el --- ERT for lightweight cdl facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'cdl)

(ert-deftest cdl-test/require-loads-standard-feature ()
  (should (featurep 'cdl))
  (should (fboundp 'cdl-get-file))
  (should (fboundp 'cdl-put-region)))

(ert-deftest cdl-test/get-file-calls-ncdump-and-restores-point ()
  (let (call)
    (with-temp-buffer
      (insert "prefix")
      (goto-char 3)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest args)
                   (setq call args)
                   (insert "dump")
                   0))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (cdl-get-file "data.nc")
        (should (= (point) 3))
        (should (equal (list (nth 0 call) (nth 1 call)
                             (nth 2 call) (nth 3 call))
                       '("ncdump" nil t nil)))
        (should (string-suffix-p "data.nc" (nth 4 call)))))))

(ert-deftest cdl-test/put-region-calls-ncgen-with-output-file ()
  (let (call)
    (with-temp-buffer
      (insert "netcdf payload")
      (cl-letf (((symbol-function 'call-process-region)
                 (lambda (&rest args)
                   (setq call args)
                   0))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (cdl-put-region "out.nc" 1 7)
        (should (equal (list (nth 0 call) (nth 1 call) (nth 2 call)
                             (nth 3 call) (nth 4 call) (nth 5 call)
                             (nth 6 call))
                       '(1 7 "ncgen" nil nil nil "-o")))
        (should (string-suffix-p "out.nc" (nth 7 call)))))))

(provide 'cdl-test)

;;; cdl-test.el ends here
