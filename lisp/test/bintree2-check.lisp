(in-package #:bintree2)

(export 'check-bintree2)
(export 'check-bintree2-error)

;;  tree
(defun check-small-binnode2 (key1 node compare)
  (or (null node)
      (let ((key (binnode2-key node))
            (left (binnode2-left node))
            (right (binnode2-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-binnode2 key1 left compare)
             (check-small-binnode2 key1 right compare)))))

(defun check-large-binnode2 (key1 node compare)
  (or (null node)
      (let ((key (binnode2-key node))
            (left (binnode2-left node))
            (right (binnode2-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-binnode2 key1 left compare)
             (check-large-binnode2 key1 right compare)))))

(defun check-tree-binnode2 (node compare)
  (or (null node)
      (let ((key (binnode2-key node))
            (left (binnode2-left node))
            (right (binnode2-right node)))
        (and (check-small-binnode2 key left compare)
             (check-large-binnode2 key right compare)
             (check-tree-binnode2 left compare)
             (check-tree-binnode2 right compare)))))

(defun check-tree-bintree2-call (inst)
  (declare (type bintree2 inst))
  (check-tree-binnode2
    (bintree2-root inst)
    (bintree2-compare inst)))

(defun check-tree-bintree2 (inst &key error)
  (let ((check (check-tree-bintree2-call inst)))
    (when (and error (null check))
      (error "check-tree-bintree2 error, ~S." inst))
    check))

(defun check-tree-bintree2-error (inst)
  (declare (type bintree2 inst))
  (check-tree-bintree2 inst :error t))


;;  parent
(defun check-parent-node-bintree2 (node)
  (or (null node)
      (let ((left (binnode2-left node))
            (right (binnode2-right node)))
        (and (or (null left) (eq (binnode2-parent left) node))
             (or (null right) (eq (binnode2-parent right) node))
             (check-parent-node-bintree2 left)
             (check-parent-node-bintree2 right)
             t))))

(defun check-parent-root-bintree2 (inst)
  (let ((root (bintree2-root inst))
        (size (bintree2-size inst)))
    (if (zerop size)
      (null root)
      (and root t))))

(defun check-parent-bintree2-call (inst)
  (and (check-parent-root-bintree2 inst)
       (check-parent-node-bintree2 (bintree2-root inst))
       t))

(defun check-parent-bintree2 (inst &key error)
  (let ((check (check-parent-bintree2-call inst)))
    (when (and error (null check))
      (error "check-parent-bintree2 error, ~S." inst))
    check))

(defun check-parent-bintree2-error (inst)
  (declare (type bintree2 inst))
  (check-parent-bintree2 inst :error t))


;;  size
(defun check-size-bintree2-map (inst)
  (declare (type bintree2 inst))
  (let ((size 0))
    (map-binnode2
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-bintree2-call (inst)
  (declare (type bintree2 inst))
  (let ((size (bintree2-size inst))
        (check (check-size-bintree2-map inst)))
    (values (= check size) check size)))

(defun check-size-bintree2 (inst &key error)
  (declare (type bintree2 inst))
  (multiple-value-bind (check count size) (check-size-bintree2-call inst)
    (when (and error (null check))
      (error "check-size-bintree2 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-bintree2-error (inst)
  (declare (type bintree2 inst))
  (check-size-bintree2 inst :error t))


;;  check
(defun check-bintree2 (inst &key error)
  (declare (type bintree2 inst))
  (and (check-tree-bintree2 inst :error error)
       (check-parent-bintree2 inst :error error)
       (check-size-bintree2 inst :error error)))

(defun check-bintree2-error (inst)
  (declare (type bintree2 inst))
  (check-bintree2 inst :error t))

