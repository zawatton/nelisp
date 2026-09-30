(pgtk-disown-selection-internal
 (pgtk-disown-selection-internal 'PRIMARY)
 (with-temp-buffer
   (insert "selection")
   (list (bufferp (current-buffer))
         (pgtk-disown-selection-internal 'CLIPBOARD nil (selected-frame)))))
(pgtk-drop-finish
 (pgtk-drop-finish t 1 nil)
 (condition-case e (pgtk-drop-finish nil nil nil) (error e)))
(pgtk-get-selection-internal
 (condition-case e (pgtk-get-selection-internal 'PRIMARY 'STRING) (error e))
 (with-temp-buffer
   (insert "changed")
   (condition-case e (pgtk-get-selection-internal 'CLIPBOARD 'STRING 1)
     (error e))))
(pgtk-own-selection-internal
 (condition-case e (pgtk-own-selection-internal 'PRIMARY "value") (error e))
 (condition-case e
     (pgtk-own-selection-internal 'CLIPBOARD
                                  (cons (copy-marker 1) (copy-marker 1))
                                  (selected-frame))
   (error e)))
(pgtk-register-dnd-targets
 (condition-case e (pgtk-register-dnd-targets (selected-frame) '("text/plain"))
   (error e))
 (let ((w (split-window)))
   (unwind-protect
       (condition-case e
           (pgtk-register-dnd-targets (window-frame w) '("text/uri-list"))
         (error e))
     (delete-window w))))
(pgtk-selection-exists-p
 (pgtk-selection-exists-p)
 (with-temp-buffer
   (insert "changed")
   (list (bufferp (current-buffer)) (pgtk-selection-exists-p 'CLIPBOARD))))
(pgtk-selection-owner-p
 (pgtk-selection-owner-p)
 (with-temp-buffer
   (insert "changed")
   (list (bufferp (current-buffer)) (pgtk-selection-owner-p 'SECONDARY))))
(pgtk-update-drop-status
 (pgtk-update-drop-status 'copy 1)
 (condition-case e (pgtk-update-drop-status 'move nil) (error e)))
