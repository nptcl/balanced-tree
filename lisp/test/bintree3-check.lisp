(in-package #:bintree3)

(export 'check-bintree3)
(export 'check-bintree3-error)

;;  tree
(defun check-small-binnode3 (key1 node compare)
  (or (null node)
      (let ((key (binnode3-key node))
            (left (binnode3-left node))
            (right (binnode3-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-binnode3 key1 left compare)
             (check-small-binnode3 key1 right compare)))))

(defun check-large-binnode3 (key1 node compare)
  (or (null node)
      (let ((key (binnode3-key node))
            (left (binnode3-left node))
            (right (binnode3-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-binnode3 key1 left compare)
             (check-large-binnode3 key1 right compare)))))

(defun check-tree-binnode3 (node compare)
  (or (null node)
      (let ((key (binnode3-key node))
            (left (binnode3-left node))
            (right (binnode3-right node)))
        (and (check-small-binnode3 key left compare)
             (check-large-binnode3 key right compare)
             (check-tree-binnode3 left compare)
             (check-tree-binnode3 right compare)))))

(defun check-tree-bintree3-call (inst)
  (declare (type bintree3 inst))
  (check-tree-binnode3
    (bintree3-root inst)
    (bintree3-compare inst)))

(defun check-tree-bintree3 (inst &key error)
  (let ((check (check-tree-bintree3-call inst)))
    (when (and error (null check))
      (error "check-tree-bintree3 error, ~S." inst))
    check))

(defun check-tree-bintree3-error (inst)
  (declare (type bintree3 inst))
  (check-tree-bintree3 inst :error t))


;;  parent
(defun check-parent-node-bintree3 (node)
  (or (null node)
      (let ((left (binnode3-left node))
            (right (binnode3-right node)))
        (and (or (null left) (eq (binnode3-parent left) node))
             (or (null right) (eq (binnode3-parent right) node))
             (check-parent-node-bintree3 left)
             (check-parent-node-bintree3 right)
             t))))

(defun check-parent-root-bintree3 (inst)
  (let ((root (bintree3-root inst))
        (size (bintree3-size inst)))
    (if (zerop size)
      (null root)
      (and root t))))

(defun check-parent-bintree3-call (inst)
  (and (check-parent-root-bintree3 inst)
       (check-parent-node-bintree3 (bintree3-root inst))
       t))

(defun check-parent-bintree3 (inst &key error)
  (let ((check (check-parent-bintree3-call inst)))
    (when (and error (null check))
      (error "check-parent-bintree3 error, ~S." inst))
    check))

(defun check-parent-bintree3-error (inst)
  (declare (type bintree3 inst))
  (check-parent-bintree3 inst :error t))


;;  size
(defun check-size-bintree3-map (inst)
  (declare (type bintree3 inst))
  (let ((size 0))
    (map-binnode3
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-bintree3-call (inst)
  (declare (type bintree3 inst))
  (let ((size (bintree3-size inst))
        (check (check-size-bintree3-map inst)))
    (values (= check size) check size)))

(defun check-size-bintree3 (inst &key error)
  (declare (type bintree3 inst))
  (multiple-value-bind (check count size) (check-size-bintree3-call inst)
    (when (and error (null check))
      (error "check-size-bintree3 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-bintree3-error (inst)
  (declare (type bintree3 inst))
  (check-size-bintree3 inst :error t))


;;  check
(defun check-bintree3 (inst &key error)
  (declare (type bintree3 inst))
  (and (check-tree-bintree3 inst :error error)
       (check-parent-bintree3 inst :error error)
       (check-size-bintree3 inst :error error)))

(defun check-bintree3-error (inst)
  (declare (type bintree3 inst))
  (check-bintree3 inst :error t))

