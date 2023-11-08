(in-package #:bintree1)

(export 'check-bintree1)
(export 'check-bintree1-error)

;;  tree
(defun check-small-binnode1 (key1 node compare)
  (or (null node)
      (let ((key (binnode1-key node))
            (left (binnode1-left node))
            (right (binnode1-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-binnode1 key1 left compare)
             (check-small-binnode1 key1 right compare)))))

(defun check-large-binnode1 (key1 node compare)
  (or (null node)
      (let ((key (binnode1-key node))
            (left (binnode1-left node))
            (right (binnode1-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-binnode1 key1 left compare)
             (check-large-binnode1 key1 right compare)))))

(defun check-tree-binnode1 (node compare)
  (or (null node)
      (let ((key (binnode1-key node))
            (left (binnode1-left node))
            (right (binnode1-right node)))
        (and (check-small-binnode1 key left compare)
             (check-large-binnode1 key right compare)
             (check-tree-binnode1 left compare)
             (check-tree-binnode1 right compare)))))

(defun check-tree-bintree1-call (inst)
  (declare (type bintree1 inst))
  (check-tree-binnode1
    (bintree1-root inst)
    (bintree1-compare inst)))

(defun check-tree-bintree1 (inst &key error)
  (let ((check (check-tree-bintree1-call inst)))
    (when (and error (null check))
      (error "check-tree-bintree1 error, ~S." inst))
    check))

(defun check-tree-bintree1-error (inst)
  (declare (type bintree1 inst))
  (check-tree-bintree1 inst :error t))


;;  size
(defun check-size-bintree1-map (inst)
  (declare (type bintree1 inst))
  (let ((size 0))
    (map-binnode1
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-bintree1-call (inst)
  (declare (type bintree1 inst))
  (let ((size (bintree1-size inst))
        (check (check-size-bintree1-map inst)))
    (values (= check size) check size)))

(defun check-size-bintree1 (inst &key error)
  (declare (type bintree1 inst))
  (multiple-value-bind (check count size) (check-size-bintree1-call inst)
    (when (and error (null check))
      (error "check-size-bintree1 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-bintree1-error (inst)
  (declare (type bintree1 inst))
  (check-size-bintree1 inst :error t))


;;  check
(defun check-bintree1 (inst &key error)
  (declare (type bintree1 inst))
  (and (check-tree-bintree1 inst :error error)
       (check-size-bintree1 inst :error error)))

(defun check-bintree1-error (inst)
  (declare (type bintree1 inst))
  (check-bintree1 inst :error t))

