(in-package #:rbtree2)

(export 'check-rbtree2)
(export 'check-rbtree2-error)

;;  root
(defun check-root-rbtree2-call (inst)
  (declare (type rbtree2 inst))
  (let ((root (rbtree2-root inst)))
    (or (and (null root)
             (eql (rbtree2-size inst) 0))
        (eq (rbnode2-color root) 'black))))

(defun check-root-rbtree2 (inst &key error)
  (declare (type rbtree2 inst))
  (let ((check (check-root-rbtree2-call inst)))
    (when (and error (null check))
      (error "check-root-rbtree2 error, ~S, ~A." inst check))
    check))

(defun check-root-rbtree2-error (inst)
  (declare (type rbtree2 inst))
  (check-root-rbtree2 inst :error t))


;;  parent
(defun check-parent-node-rbtree2 (node)
  (or (null node)
      (let ((left (rbnode2-left node))
            (right (rbnode2-right node)))
        (and (or (null left) (eq (rbnode2-parent left) node))
             (or (null right) (eq (rbnode2-parent right) node))
             (check-parent-node-rbtree2 left)
             (check-parent-node-rbtree2 right)
             t))))

(defun check-parent-root-rbtree2 (inst)
  (let ((root (rbtree2-root inst))
        (size (rbtree2-size inst)))
    (if (zerop size)
      (null root)
      (and root t))))

(defun check-parent-rbtree2-call (inst)
  (and (check-parent-root-rbtree2 inst)
       (check-parent-node-rbtree2 (rbtree2-root inst))
       t))

(defun check-parent-rbtree2 (inst &key error)
  (let ((check (check-parent-rbtree2-call inst)))
    (when (and error (null check))
      (error "check-parent-rbtree2 error, ~S." inst))
    check))

(defun check-parent-rbtree2-error (inst)
  (declare (type rbtree2 inst))
  (check-parent-rbtree2 inst :error t))


;;  red node
(defun check-red-rbtree2-node (node)
  (let ((left (rbnode2-left node))
        (right (rbnode2-right node))
        (color (rbnode2-color node)))
    (or (eq color 'black)
        (and (eq color 'red)
             (or (null left) (eq (rbnode2-color left) 'black))
             (or (null right) (eq (rbnode2-color right) 'black))))))

(defun check-red-rbtree2-call (inst)
  (declare (type rbtree2 inst))
  (block nil
    (map-rbnode2
      (lambda (node)
        (unless (check-red-rbtree2-node node)
          (return nil)))
      inst)
    t))

(defun check-red-rbtree2 (inst &key error)
  (declare (type rbtree2 inst))
  (let ((check (check-red-rbtree2-call inst)))
    (when (and error (null check))
      (error "check-red-rbtree2 error, ~S, ~A." inst check))
    check))

(defun check-red-rbtree2-error (inst)
  (declare (type rbtree2 inst))
  (check-red-rbtree2 inst :error t))


;;  compare
(defun check-compare-rbtree2-compare (node call)
  (let ((left (rbnode2-left node))
        (right (rbnode2-right node)))
    (when left
      (check-compare-rbtree2-compare left call))
    (funcall call node)
    (when right
      (check-compare-rbtree2-compare right call))))

(defun check-compare-rbtree2-call (inst)
  (block nil
    (let ((compare (rbtree2-compare inst))
          (root (rbtree2-root inst))
          x y)
      (when root
        (check-compare-rbtree2-compare
          root
          (lambda (node)
            (setq x y)
            (setq y (rbnode2-key node))
            (when x
              (unless (funcall compare x y)
                (return nil)))))))
    t))

(defun check-compare-rbtree2 (inst &key error)
  (declare (type rbtree2 inst))
  (let ((check (check-compare-rbtree2-call inst)))
    (when (and error (null check))
      (error "check-compare-rbtree2 error, ~S, ~A." inst check))
    check))

(defun check-compare-rbtree2-error (inst)
  (declare (type rbtree2 inst))
  (check-compare-rbtree2 inst :error t))


;;  length
(defun check-length-rbtree2-apply (node call &optional (count 0))
  (let* ((left (rbnode2-left node))
         (right (rbnode2-right node))
         (check1 (null left))
         (check2 (null right))
         (count1 (1+ count)))
    (when (and check1 check2)
      (funcall call count))
    (unless check1
      (check-length-rbtree2-apply left call count1))
    (unless check2
      (check-length-rbtree2-apply right call count1))))

(defun check-length-rbtree2-maxmin (inst)
  (let ((root (rbtree2-root inst))
        (max 0)
        (min 0))
    (check-length-rbtree2-apply
      root
      (lambda (count)
        (setq max (max max count))
        (setq min (min min count))))
    (values max min)))

(defun check-length-rbtree2-size (inst)
  (let ((size (rbtree2-size inst)))
    (if (eql size 0)
      (values size size)
      (check-length-rbtree2-maxmin inst))))

(defun check-length-rbtree2-call (inst)
  (multiple-value-bind (x y) (check-length-rbtree2-size inst)
    (values
      (or (and (zerop x) (zerop y))
          (and (eql x 1) (eql y 1))
          (<= y (* 2 x)))
      x y)))

(defun check-length-rbtree2 (inst &key error)
  (declare (type rbtree2 inst))
  (multiple-value-bind (check x y) (check-length-rbtree2-call inst)
    (when (and error (null check))
      (error "check-length-rbtree2 error, ~S, ~A, ~A, ~A." inst check x y))
    check))

(defun check-length-rbtree2-error (inst)
  (declare (type rbtree2 inst))
  (check-length-rbtree2 inst :error t))


;;  size
(defun check-size-rbtree2-map (inst)
  (declare (type rbtree2 inst))
  (let ((size 0))
    (map-rbnode2
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-rbtree2-call (inst)
  (declare (type rbtree2 inst))
  (let ((size (rbtree2-size inst))
        (check (check-size-rbtree2-map inst)))
    (values (= check size) check size)))

(defun check-size-rbtree2 (inst &key error)
  (declare (type rbtree2 inst))
  (multiple-value-bind (check count size) (check-size-rbtree2-call inst)
    (when (and error (null check))
      (error "check-size-rbtree2 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-rbtree2-error (inst)
  (declare (type rbtree2 inst))
  (check-size-rbtree2 inst :error t))


;;  check
(defun check-rbtree2 (inst &key error)
  (declare (type rbtree2 inst))
  (and (check-root-rbtree2 inst :error error)
       (check-parent-rbtree2 inst :error error)
       (check-red-rbtree2 inst :error error)
       (check-compare-rbtree2 inst :error error)
       (check-length-rbtree2 inst :error error)
       (check-size-rbtree2 inst :error error)
       t))

(defun check-rbtree2-error (inst)
  (declare (type rbtree2 inst))
  (check-rbtree2 inst :error t))

