(clear-face-cache
 (clear-face-cache)
 (condition-case e (clear-face-cache 7) (error (list (car e) (cdr e))))
 (let ((b (get-buffer-create " *xfaces-probe*"))) (with-current-buffer b (insert "x")) (prog1 (list (buffer-live-p b) (= (buffer-size b) 1)) (kill-buffer b)))
)
(color-distance
 (color-distance "red" "blue")
 (color-distance "black" "white")
 (condition-case e (color-distance nil "blue") (error (list (car e) (cdr e))))
 (color-distance "red" "blue" nil (lambda (a b) (list (car a) (cadr b))))
)
(color-gray-p
 (list (color-gray-p "gray50") (color-gray-p "red"))
 (condition-case e (color-gray-p nil) (error (list (car e) (cdr e))))
 (let ((w (split-window))) (prog1 (list (color-gray-p "white") (windowp w)) (delete-window w)))
)
(color-supported-p
 (list (color-supported-p "red") (color-supported-p "not-a-real-color"))
 (condition-case e (color-supported-p nil) (error (list (car e) (cdr e))))
 (list (color-supported-p "#abc" nil t) (color-supported-p "rgb:ff/00/00"))
)
(color-values-from-color-spec
 (color-values-from-color-spec "#f00")
 (color-values-from-color-spec "rgbi:1/0.5/0")
 (color-values-from-color-spec "not-a-color")
 (condition-case e (color-values-from-color-spec nil) (error (list (car e) (cdr e))))
)
(face-attributes-as-vector
 (aref (face-attributes-as-vector nil) 0)
 (aref (face-attributes-as-vector '(:weight bold :foreground "red")) 5)
 (aref (face-attributes-as-vector '(:height 140 :underline t)) 7)
 (face-attributes-as-vector 1)
)
(frame--face-hash-table
 (hash-table-p (frame--face-hash-table))
 (let ((w (split-window))) (prog1 (hash-table-p (frame--face-hash-table (window-frame w))) (delete-window w)))
 (condition-case e (frame--face-hash-table 1) (error (list (car e) (cdr e))))
)
(internal-set-alternative-font-family-alist
 (internal-set-alternative-font-family-alist '(("sans" "serif")))
 (condition-case e (internal-set-alternative-font-family-alist 1) (error (list (car e) (cdr e))))
 (internal-set-alternative-font-family-alist nil)
)
(internal-set-alternative-font-registry-alist
 (internal-set-alternative-font-registry-alist '(("iso" "unicode")))
 (condition-case e (internal-set-alternative-font-registry-alist 1) (error (list (car e) (cdr e))))
 (internal-set-alternative-font-registry-alist nil)
)
(internal-set-font-selection-order
 (internal-set-font-selection-order '(:width :height :weight :slant))
 (internal-set-font-selection-order '(:weight :slant :height :width))
 (condition-case e (internal-set-font-selection-order nil) (error (list (car e) (cdr e))))
)
(internal-set-lisp-face-attribute-from-resource
 (condition-case e (internal-set-lisp-face-attribute-from-resource 'default :weight 'bold) (error (list (car e) (cdr e))))
 (condition-case e (internal-set-lisp-face-attribute-from-resource 1 :weight 'bold) (error (list (car e) (cdr e))))
 (let ((w (split-window))) (prog1 (condition-case e (internal-set-lisp-face-attribute-from-resource 'default :height 120 (window-frame w)) (error (list (car e) (cdr e)))) (delete-window w)))
)
(tty-suppress-bold-inverse-default-colors
 (tty-suppress-bold-inverse-default-colors t)
 (tty-suppress-bold-inverse-default-colors nil)
 (let ((b (get-buffer-create " *xfaces-probe*"))) (with-current-buffer b (insert (propertize "x" 'face 'bold))) (prog1 (list (buffer-live-p b) (get-text-property 1 'face b)) (kill-buffer b)))
)
