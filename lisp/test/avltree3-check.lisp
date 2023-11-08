(in-package #:avltree3)

(export 'check-avltree3)
(export 'check-avltree3-error)

;;  balanced
(defun check-balanced-avlnode3 (node)
  (if (null node)
    0
    (let* ((left (avlnode3-left node))
           (right (avlnode3-right node))
           (balance (avlnode3-balance node))
           (x (check-balanced-avlnode3 left))
           (y (check-balanced-avlnode3 right))
           (z (- x y)))
      (unless (= balance z)
        (error "balance error, ~S, ~S." balance z))
      (1+ (max x y)))))

(defun check-balanced-avltree3 (inst &key error)
  (declare (type avltree3 inst))
  (let ((root (avltree3-root inst)))
    (if error
      (check-balanced-avlnode3 root)
      (handler-case
        (progn (check-balanced-avlnode3 root) t)
        (error () nil)))))

(defun check-balanced-avltree3-error (inst)
  (declare (type avltree3 inst))
  (check-balanced-avltree3 inst :error t))


;;  parent
(defun check-parent-node-avltree3 (node)
  (or (null node)
      (let ((left (avlnode3-left node))
            (right (avlnode3-right node)))
        (and (or (null left) (eq (avlnode3-parent left) node))
             (or (null right) (eq (avlnode3-parent right) node))
             (check-parent-node-avltree3 left)
             (check-parent-node-avltree3 right)
             t))))

(defun check-parent-root-avltree3 (inst)
  (let ((root (avltree3-root inst))
        (size (avltree3-size inst)))
    (if (zerop size)
      (null root)
      (and root t))))

(defun check-parent-avltree3-call (inst)
  (and (check-parent-root-avltree3 inst)
       (check-parent-node-avltree3 (avltree3-root inst))
       t))

(defun check-parent-avltree3 (inst &key error)
  (let ((check (check-parent-avltree3-call inst)))
    (when (and error (null check))
      (error "check-parent-avltree3 error, ~S." inst))
    check))

(defun check-parent-avltree3-error (inst)
  (declare (type avltree3 inst))
  (check-parent-avltree3 inst :error t))


;;  tree
(defun check-small-avlnode3 (key1 node compare)
  (or (null node)
      (let ((key (avlnode3-key node))
            (left (avlnode3-left node))
            (right (avlnode3-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-avlnode3 key1 left compare)
             (check-small-avlnode3 key1 right compare)))))

(defun check-large-avlnode3 (key1 node compare)
  (or (null node)
      (let ((key (avlnode3-key node))
            (left (avlnode3-left node))
            (right (avlnode3-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-avlnode3 key1 left compare)
             (check-large-avlnode3 key1 right compare)))))

(defun check-tree-avlnode3 (node compare)
  (or (null node)
      (let ((key (avlnode3-key node))
            (left (avlnode3-left node))
            (right (avlnode3-right node)))
        (and (check-small-avlnode3 key left compare)
             (check-large-avlnode3 key right compare)
             (check-tree-avlnode3 left compare)
             (check-tree-avlnode3 right compare)))))

(defun check-tree-avltree3-call (inst)
  (declare (type avltree3 inst))
  (check-tree-avlnode3
    (avltree3-root inst)
    (avltree3-compare inst)))

(defun check-tree-avltree3 (inst &key error)
  (let ((check (check-tree-avltree3-call inst)))
    (when (and error (null check))
      (error "check-tree-avltree3 error, ~S." inst))
    check))

(defun check-tree-avltree3-error (inst)
  (declare (type avltree3 inst))
  (check-tree-avltree3 inst :error t))


;;  size
(defun check-size-avltree3-map (inst)
  (declare (type avltree3 inst))
  (let ((size 0))
    (map-avlnode3
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-avltree3-call (inst)
  (declare (type avltree3 inst))
  (let ((size (avltree3-size inst))
        (check (check-size-avltree3-map inst)))
    (values (= check size) check size)))

(defun check-size-avltree3 (inst &key error)
  (declare (type avltree3 inst))
  (multiple-value-bind (check count size) (check-size-avltree3-call inst)
    (when (and error (null check))
      (error "check-size-avltree3 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-avltree3-error (inst)
  (declare (type avltree3 inst))
  (check-size-avltree3 inst :error t))


;;  check
(defun check-avltree3 (inst &key error)
  (declare (type avltree3 inst))
  (and (check-balanced-avltree3 inst :error error)
       (check-parent-avltree3 inst :error error)
       (check-tree-avltree3 inst :error error)
       (check-size-avltree3 inst :error error)))

(defun check-avltree3-error (inst)
  (declare (type avltree3 inst))
  (check-avltree3 inst :error t))

