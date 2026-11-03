;;; emacs-cc-image-1.el --- image C-core compatibility -*- lexical-binding: t; -*-
(defun emacs-cc-image-1--valid-spec-p (spec)
  "Return non-nil when SPEC has the shape of a supported image spec."
  (and (consp spec) (eq (car spec) 'image)
       (let ((tail (cdr spec)) (type nil) (valid t))
         (while (and tail valid)
           (if (and (consp tail) (keywordp (car tail)) (consp (cdr tail)))
               (progn (when (eq (car tail) :type) (setq type (cadr tail)))
                      (setq tail (cddr tail)))
             (setq valid nil)))
         (and valid (memq type '(png jpeg gif tiff xpm svg pbm
                                      imagemagick postscript))))))
(defun emacs-cc-image-1--display-error ()
  (signal 'error '("Window system frame should be used")))
(unless (fboundp 'clear-image-cache)
  (defun clear-image-cache (&optional filter animation-filter)
    "Clear the image and animation caches.
FILTER nil or a frame means clear all images in the selected frame.
FILTER t means clear the image caches of all frames.
Anything else means clear only those images that refer to FILTER,
which is then usually a filename.

This function also clears the image animation cache.
ANIMATION-FILTER nil means clear all animation cache entries.
Otherwise, clear the image spec eq to ANIMATION-FILTER only
from the animation cache, and do not clear any image caches.
This can help reduce memory usage after an animation is stopped
but the image is still displayed."
    (when animation-filter
      (unless (consp animation-filter)
        (signal 'wrong-type-argument (list 'consp animation-filter))))
    (if (or (null filter) (eq filter t))
        (emacs-cc-image-1--display-error)
      nil)))
(unless (fboundp 'image-cache-size)
  (defun image-cache-size () "Return the size of the image cache." 0))
(unless (fboundp 'image-flush)
  (defun image-flush (spec &optional frame)
    "Flush the image with specification SPEC on frame FRAME.
This removes the image from the Emacs image cache.  If SPEC specifies
an image file, the next redisplay of this image will read from the
current contents of that file.

FRAME nil or omitted means use the selected frame.
FRAME t means refresh the image on all frames."
    (ignore frame)
    (unless (emacs-cc-image-1--valid-spec-p spec)
      (signal 'error '("Invalid image specification")))
    (emacs-cc-image-1--display-error)))
(unless (fboundp 'image-mask-p)
  (defun image-mask-p (spec &optional frame)
    "Return t if image SPEC has a mask bitmap.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame."
    (ignore frame)
    (if (not (emacs-cc-image-1--valid-spec-p spec))
        (signal 'error '("Invalid image specification"))
      (emacs-cc-image-1--display-error))))
(unless (fboundp 'image-metadata)
  (defun image-metadata (spec &optional frame)
    "Return metadata for image SPEC.
FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame."
    (ignore frame)
    (when (emacs-cc-image-1--valid-spec-p spec)
      (emacs-cc-image-1--display-error))
    nil))
(unless (fboundp 'imagep)
  (defun imagep (spec)
    "Value is non-nil if SPEC is a valid image specification."
    (and (emacs-cc-image-1--valid-spec-p spec) t)))
(unless (fboundp 'image-size)
  (defun image-size (spec &optional pixels frame)
    "Return the size of image SPEC as pair (WIDTH . HEIGHT).
PIXELS non-nil means return the size in pixels, otherwise return the
size in canonical character units.

FRAME is the frame on which the image will be displayed.  FRAME nil
or omitted means use the selected frame.

Calling this function will result in the image being stored in the image
cache.  If this is not desirable, call image-flush after calling this function."
    (ignore pixels frame)
    (unless (emacs-cc-image-1--valid-spec-p spec)
      (signal 'error '("Invalid image specification")))
    (emacs-cc-image-1--display-error)))
(unless (fboundp 'image-transforms-p)
  (defun image-transforms-p (&optional frame)
    "Test whether FRAME supports image transformation.
Return list of capabilities if FRAME supports native transforms, nil otherwise.
FRAME defaults to the selected frame."
    (when (and frame (not (eq frame t)) (not (frame-live-p frame)))
      (signal 'wrong-type-argument (list 'frame-live-p frame)))
    nil))
(unless (fboundp 'init-image-library)
  (defun init-image-library (type)
    "Initialize image library implementing image type TYPE.
Return t if TYPE is a supported image type.

If image libraries are loaded dynamically (currently the case only on
MS-Windows), load the library for TYPE if it is not yet loaded, using
the library file(s) specified by dynamic-library-alist."
    ;; No ImageMagick or Ghostscript backend exists here.  Claiming them
    ;; made treemacs choose `imagemagick' icons and fail in `create-image';
    ;; GNU built without those libraries answers nil as well.
    (and (memq type '(png jpeg gif tiff xpm svg pbm)) t)))
(provide 'emacs-cc-image-1)
