;;; treesit-3.el --- treesit-3 C primitive probes
(treesit-node-start (treesit-node-start nil)
                     (condition-case e (treesit-node-start 'bad) (error e)))
 (treesit-node-string (treesit-node-string nil)
                      (condition-case e (treesit-node-string 'bad) (error e)))
 (treesit-node-type (treesit-node-type nil)
                    (condition-case e (treesit-node-type 'bad) (error e)))
 (treesit-parser-add-notifier
  (condition-case e (treesit-parser-add-notifier nil 'ignore) (error e))
  (condition-case e (treesit-parser-add-notifier nil (lambda (_r _p) nil)) (error e)))
 (treesit-parser-buffer
  (condition-case e (treesit-parser-buffer nil) (error e))
  (condition-case e (treesit-parser-buffer 'bad) (error e)))
 (treesit-parser-changed-regions
  (condition-case e (treesit-parser-changed-regions nil) (error e))
  (condition-case e (treesit-parser-changed-regions 'bad) (error e)))
 (treesit-parser-create
  (condition-case nil (progn (treesit-parser-create 'json) nil) (error t))
  (condition-case nil (progn (treesit-parser-create 'made-up-language (current-buffer) t 'tag) nil) (error t)))
 (treesit-parser-delete
  (condition-case e (treesit-parser-delete nil) (error e))
  (condition-case e (treesit-parser-delete 'bad) (error e)))
 (treesit-parser-embed-level
  (condition-case e (treesit-parser-embed-level nil) (error e))
  (condition-case e (treesit-parser-embed-level 'bad) (error e)))
 (treesit-parser-included-ranges
  (condition-case e (treesit-parser-included-ranges nil) (error e))
  (condition-case e (treesit-parser-included-ranges 'bad) (error e)))
 (treesit-parser-language
  (condition-case e (treesit-parser-language nil) (error e))
  (condition-case e (treesit-parser-language 'bad) (error e)))
 (treesit-parser-list
  (list (length (treesit-parser-list)) (treesit-parser-list (current-buffer) 'json t))
  (let ((b (generate-new-buffer " *treesit-probe*")))
    (unwind-protect (list (bufferp b) (treesit-parser-list b 'json 'tag))
      (kill-buffer b)))
  (condition-case e (treesit-parser-list 'bad) (error e)))
