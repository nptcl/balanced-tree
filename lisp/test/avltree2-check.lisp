(in-package #:avltree2)

(export 'check-avltree2)
(export 'check-avltree2-error)

;;  balanced
(defun check-balanced-avlnode2 (node)
  (if (null node)
    0
    (let* ((left (avlnode2-left node))
           (right (avlnode2-right node))
           (balance (avlnode2-balance node))
           (x (check-balanced-avlnode2 left))
           (y (check-balanced-avlnode2 right))
           (z (- x y)))
      (unless (= balance z)
        (error "balance error, ~S, ~S." balance z))
      (1+ (max x y)))))

(defun check-balanced-avltree2 (inst &key error)
  (declare (type avltree2 inst))
  (let ((root (avltree2-root inst)))
    (if error
      (check-balanced-avlnode2 root)
      (handler-case
        (progn (check-balanced-avlnode2 root) t)
        (error () nil)))))

(defun check-balanced-avltree2-error (inst)
  (declare (type avltree2 inst))
  (check-balanced-avltree2 inst :error t))


;;  parent
(defun check-parent-node-avltree2 (node)
  (or (null node)
      (let ((left (avlnode2-left node))
            (right (avlnode2-right node)))
        (and (or (null left) (eq (avlnode2-parent left) node))
             (or (null right) (eq (avlnode2-parent right) node))
             (check-parent-node-avltree2 left)
             (check-parent-node-avltree2 right)
             t))))

(defun check-parent-root-avltree2 (inst)
  (let ((root (avltree2-root inst))
        (size (avltree2-size inst)))
    (if (zerop size)
      (null root)
      (and root t))))

(defun check-parent-avltree2-call (inst)
  (and (check-parent-root-avltree2 inst)
       (check-parent-node-avltree2 (avltree2-root inst))
       t))

(defun check-parent-avltree2 (inst &key error)
  (let ((check (check-parent-avltree2-call inst)))
    (when (and error (null check))
      (error "check-parent-avltree2 error, ~S." inst))
    check))

(defun check-parent-avltree2-error (inst)
  (declare (type avltree2 inst))
  (check-parent-avltree2 inst :error t))


;;  tree
(defun check-small-avlnode2 (key1 node compare)
  (or (null node)
      (let ((key (avlnode2-key node))
            (left (avlnode2-left node))
            (right (avlnode2-right node)))
        (and (< 0 (funcall compare key1 key))
             (check-small-avlnode2 key1 left compare)
             (check-small-avlnode2 key1 right compare)))))

(defun check-large-avlnode2 (key1 node compare)
  (or (null node)
      (let ((key (avlnode2-key node))
            (left (avlnode2-left node))
            (right (avlnode2-right node)))
        (and (< (funcall compare key1 key) 0)
             (check-large-avlnode2 key1 left compare)
             (check-large-avlnode2 key1 right compare)))))

(defun check-tree-avlnode2 (node compare)
  (or (null node)
      (let ((key (avlnode2-key node))
            (left (avlnode2-left node))
            (right (avlnode2-right node)))
        (and (check-small-avlnode2 key left compare)
             (check-large-avlnode2 key right compare)
             (check-tree-avlnode2 left compare)
             (check-tree-avlnode2 right compare)))))

(defun check-tree-avltree2-call (inst)
  (declare (type avltree2 inst))
  (check-tree-avlnode2
    (avltree2-root inst)
    (avltree2-compare inst)))

(defun check-tree-avltree2 (inst &key error)
  (let ((check (check-tree-avltree2-call inst)))
    (when (and error (null check))
      (error "check-tree-avltree2 error, ~S." inst))
    check))

(defun check-tree-avltree2-error (inst)
  (declare (type avltree2 inst))
  (check-tree-avltree2 inst :error t))


;;  size
(defun check-size-avltree2-map (inst)
  (declare (type avltree2 inst))
  (let ((size 0))
    (map-avlnode2
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-avltree2-call (inst)
  (declare (type avltree2 inst))
  (let ((size (avltree2-size inst))
        (check (check-size-avltree2-map inst)))
    (values (= check size) check size)))

(defun check-size-avltree2 (inst &key error)
  (declare (type avltree2 inst))
  (multiple-value-bind (check count size) (check-size-avltree2-call inst)
    (when (and error (null check))
      (error "check-size-avltree2 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-avltree2-error (inst)
  (declare (type avltree2 inst))
  (check-size-avltree2 inst :error t))


;;  check
(defun check-avltree2 (inst &key error)
  (declare (type avltree2 inst))
  (and (check-balanced-avltree2 inst :error error)
       (check-parent-avltree2 inst :error error)
       (check-tree-avltree2 inst :error error)
       (check-size-avltree2 inst :error error)))

(defun check-avltree2-error (inst)
  (declare (type avltree2 inst))
  (check-avltree2 inst :error t))

