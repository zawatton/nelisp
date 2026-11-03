(clear-image-cache
 (progn (clear-image-cache) (image-cache-size))
 (progn (let ((w (split-window))) (unwind-protect (clear-image-cache w) (delete-window w)))
        (image-cache-size))
 (clear-image-cache t 'animation))
(image-cache-size
 (image-cache-size)
 (progn (let ((b (get-buffer-create "image-cache-probe"))) (with-current-buffer b (insert "x")))
        (image-cache-size)))
(image-flush
 (condition-case e (image-flush nil) (error e))
 (condition-case e (image-flush '(image :type png :file "/no/such/image.png")) (error e))
 (condition-case e (image-flush nil 42) (error e)))
(image-mask-p
 (condition-case e (image-mask-p nil) (error e))
 (condition-case e (image-mask-p '(image :type png :file "/no/such/image.png")) (error e))
 (condition-case e (image-mask-p "bad") (error e)))
(image-metadata
 (image-metadata nil)
 (condition-case e (image-metadata '(image :type png :file "/no/such/image.png")) (error e))
 (image-metadata '(not-image)))
(imagep
 (imagep nil)
 (imagep '(image :type png :file "/no/such/image.png"))
 (imagep '(image :type unsupported-image-type)))
(image-size
 (condition-case e (image-size nil) (error e))
 (condition-case e (image-size '(image :type png :file "/no/such/image.png") t) (error e))
 (condition-case e (image-size '(image :type unsupported-image-type)) (error e)))
(image-transforms-p
 (image-transforms-p)
 (let ((w (split-window))) (unwind-protect (list (windowp w) (image-transforms-p)) (delete-window w)))
 (condition-case e (image-transforms-p 42) (error e)))
(init-image-library
 (init-image-library 'png)
 (init-image-library 'unsupported-image-type)
 (condition-case e (init-image-library "png") (error e)))
