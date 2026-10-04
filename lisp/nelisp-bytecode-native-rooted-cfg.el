;;; nelisp-bytecode-native-rooted-cfg.el --- CFG topology checks -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defconst nelisp-bytecode-native-rooted-cfg-max-blocks 12)
(defconst nelisp-bytecode-native-rooted-cfg-max-paths 256)

(defun nelisp-bytecode-native-rooted-cfg-topology-check (frame)
  "Check bounded acyclic FRAME topology; return order and branch polarities."
  (let* ((blocks (and (listp frame) (eq (plist-get frame :status) 'complete)
                      (plist-get frame :blocks)))
         (by-id (make-hash-table :test 'eql))
         (indegree (make-hash-table :test 'eql))
         (paths (make-hash-table :test 'eql))
         (queue nil) (order nil) (branches nil) (reason nil))
    (let ((result
           (catch 'refuse
	     (unless (and (vectorp blocks) (> (length blocks) 0)
			  (<= (length blocks) nelisp-bytecode-native-rooted-cfg-max-blocks))
               (setq reason "frame block count is outside topology limit")
               (throw 'refuse nil))
	     (dotimes (i (length blocks))
               (let* ((block (aref blocks i))
                      (id (and (listp block) (plist-get block :start))))
		 (unless (and (listp block) (vectorp (plist-get block :instructions))
			      (vectorp (plist-get block :successors))
			      (integerp id) (not (gethash id by-id)))
		   (setq reason "duplicate or invalid block id") (throw 'refuse nil))
		 (puthash id block by-id) (puthash id 0 indegree) (puthash id 0 paths)))
	     (dotimes (i (length blocks))
               (let ((block (aref blocks i)))
		 (dolist (edge (append (plist-get block :successors) nil))
		   (let ((target (and (listp edge) (plist-get edge :target))))
		     (unless (and (listp edge) (vectorp (plist-get edge :slots))
				  (vectorp (plist-get edge :target-slots))
				  (gethash target by-id)
				  (= (length (plist-get edge :slots))
				     (length (plist-get edge :target-slots))))
                       (setq reason "edge target or stack-slot transfer is invalid")
                       (throw 'refuse nil))
		     (puthash target (1+ (gethash target indegree)) indegree)))
		 (dolist (ins (append (plist-get block :instructions) nil))
		   (unless (listp ins)
		     (setq reason "invalid instruction record") (throw 'refuse nil))
		   (when (memq (plist-get ins :opcode) '(131 132))
		     (unless (= (length (plist-get block :successors)) 2)
                       (setq reason "conditional branch lacks two successors")
                       (throw 'refuse nil))
		     (push (list :pc (plist-get ins :pc) :opcode (plist-get ins :opcode)
				 :taken (if (= (plist-get ins :opcode) 131) 'nilp 'not-nilp))
			   branches)))))
	     (let ((entry (plist-get (aref blocks 0) :start)) (overflow nil))
               (unless (= (gethash entry indegree) 0)
		 (setq reason "entry block has incoming edge") (throw 'refuse nil))
               (setq queue (list entry)) (puthash entry 1 paths)
               (while queue
		 (let* ((id (pop queue)) (block (gethash id by-id)))
		   (push id order)
		   (dolist (edge (append (plist-get block :successors) nil))
		     (let* ((target (plist-get edge :target))
			    (count (+ (gethash target paths) (gethash id paths))))
                       (when (> count nelisp-bytecode-native-rooted-cfg-max-paths)
			 (setq overflow t))
                       (puthash target (min count (1+ nelisp-bytecode-native-rooted-cfg-max-paths)) paths)
                       (puthash target (1- (gethash target indegree)) indegree)
                       (when (= (gethash target indegree) 0)
			 (setq queue (append queue (list target))))))))
               (unless (and (not overflow) (= (length order) (length blocks)))
		 (setq reason (if overflow "path count exceeds topology limit"
				"frame topology is cyclic or unreachable"))
		 (throw 'refuse nil))
               (let ((total-paths
		      (apply #'+ (mapcar (lambda (id)
					   (if (= (length (plist-get (gethash id by-id) :successors)) 0)
					       (gethash id paths) 0)) order))))
		 (when (> total-paths nelisp-bytecode-native-rooted-cfg-max-paths)
		   (setq reason "total path count exceeds topology limit")
		   (throw 'refuse nil))
		 (list :status 'complete :scope 'topology-only
		       :block-order (reverse order) :branches (nreverse branches)
		       :path-count total-paths
		       :max-blocks nelisp-bytecode-native-rooted-cfg-max-blocks
		       :max-paths nelisp-bytecode-native-rooted-cfg-max-paths))))))
      (or result (list :status 'unsupported :scope 'topology-only
                       :reason (or reason "invalid frame"))))))

(provide 'nelisp-bytecode-native-rooted-cfg)
;;; nelisp-bytecode-native-rooted-cfg.el ends here
