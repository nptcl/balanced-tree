(in-package #:avltree1)

(export 'check-avltree1)
(export 'check-avltree1-error)

;;  balanced
(defun check-balanced-avlnode1 (node)
  (if (null node)
    0
    (let* ((left (avlnode1-left node))
           (right (avlnode1-right node))
           (balance (avlnode1-balance node))
           (x (check-balanced-avlnode1 left))
           (y (check-balanced-avlnode1 right))
           (z (- x y)))
      (unless (= balance z)
        (error "balance error, ~S, ~S." balance z))
      (1+ (max x y)))))

(defun check-balanced-avltree1 (inst &key error)
  (declare (type avltree1 inst))
  (let ((root (avltree1-root inst)))
    (if error
      (check-balanced-avlnode1 root)
      (handler-case
        (progn (check-balanced-avlnode1 root) t)
        (error () nil)))))

(defun check-balanced-avltree1-error (inst)
  (declare (type avltree1 inst))
  (check-balanced-avltree1 inst :error t))


;;  tree
(defun check-small-avlnode1 (key1 node compare)
  (or (null node)
      (let ((key (avlnode1-key node))
            (left (avlnode1-left node))
            (right (avlnode1-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-avlnode1 key1 left compare)
             (check-small-avlnode1 key1 right compare)))))

(defun check-large-avlnode1 (key1 node compare)
  (or (null node)
      (let ((key (avlnode1-key node))
            (left (avlnode1-left node))
            (right (avlnode1-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-avlnode1 key1 left compare)
             (check-large-avlnode1 key1 right compare)))))

(defun check-tree-avlnode1 (node compare)
  (or (null node)
      (let ((key (avlnode1-key node))
            (left (avlnode1-left node))
            (right (avlnode1-right node)))
        (and (check-small-avlnode1 key left compare)
             (check-large-avlnode1 key right compare)
             (check-tree-avlnode1 left compare)
             (check-tree-avlnode1 right compare)))))

(defun check-tree-avltree1-call (inst)
  (declare (type avltree1 inst))
  (check-tree-avlnode1
    (avltree1-root inst)
    (avltree1-compare inst)))

(defun check-tree-avltree1 (inst &key error)
  (let ((check (check-tree-avltree1-call inst)))
    (when (and error (null check))
      (error "check-tree-avltree1 error, ~S." inst))
    check))

(defun check-tree-avltree1-error (inst)
  (declare (type avltree1 inst))
  (check-tree-avltree1 inst :error t))


;;  size
(defun check-size-avltree1-map (inst)
  (declare (type avltree1 inst))
  (let ((size 0))
    (map-avlnode1
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-avltree1-call (inst)
  (declare (type avltree1 inst))
  (let ((size (avltree1-size inst))
        (check (check-size-avltree1-map inst)))
    (values (= check size) check size)))

(defun check-size-avltree1 (inst &key error)
  (declare (type avltree1 inst))
  (multiple-value-bind (check count size) (check-size-avltree1-call inst)
    (when (and error (null check))
      (error "check-size-avltree1 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-avltree1-error (inst)
  (declare (type avltree1 inst))
  (check-size-avltree1 inst :error t))


;;  check
(defun check-avltree1 (inst &key error)
  (declare (type avltree1 inst))
  (and (check-balanced-avltree1 inst :error error)
       (check-tree-avltree1 inst :error error)
       (check-size-avltree1 inst :error error)))

(defun check-avltree1-error (inst)
  (declare (type avltree1 inst))
  (check-avltree1 inst :error t))

