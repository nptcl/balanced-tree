(in-package #:rbtree3)

(export 'check-rbtree3)
(export 'check-rbtree3-error)

;;  root
(defun check-root-rbtree3-call (inst)
  (declare (type rbtree3 inst))
  (let ((root (rbtree3-root inst)))
    (or (and (null root)
             (eql (rbtree3-size inst) 0))
        (eq (rbnode3-color root) 'black))))

(defun check-root-rbtree3 (inst &key error)
  (declare (type rbtree3 inst))
  (let ((check (check-root-rbtree3-call inst)))
    (when (and error (null check))
      (error "check-root-rbtree3 error, ~S, ~A." inst check))
    check))

(defun check-root-rbtree3-error (inst)
  (declare (type rbtree3 inst))
  (check-root-rbtree3 inst :error t))


;;  parent
(defun check-parent-node-rbtree3 (node)
  (or (null node)
      (let ((left (rbnode3-left node))
            (right (rbnode3-right node)))
        (and (or (null left) (eq (rbnode3-parent left) node))
             (or (null right) (eq (rbnode3-parent right) node))
             (check-parent-node-rbtree3 left)
             (check-parent-node-rbtree3 right)
             t))))

(defun check-parent-root-rbtree3 (inst)
  (let ((root (rbtree3-root inst))
        (size (rbtree3-size inst)))
    (if (zerop size)
      (null root)
      (and root t))))

(defun check-parent-rbtree3-call (inst)
  (and (check-parent-root-rbtree3 inst)
       (check-parent-node-rbtree3 (rbtree3-root inst))
       t))

(defun check-parent-rbtree3 (inst &key error)
  (let ((check (check-parent-rbtree3-call inst)))
    (when (and error (null check))
      (error "check-parent-rbtree3 error, ~S." inst))
    check))

(defun check-parent-rbtree3-error (inst)
  (declare (type rbtree3 inst))
  (check-parent-rbtree3 inst :error t))


;;  red node
(defun check-red-rbtree3-node (node)
  (let ((left (rbnode3-left node))
        (right (rbnode3-right node))
        (color (rbnode3-color node)))
    (or (eq color 'black)
        (and (eq color 'red)
             (or (null left) (eq (rbnode3-color left) 'black))
             (or (null right) (eq (rbnode3-color right) 'black))))))

(defun check-red-rbtree3-call (inst)
  (declare (type rbtree3 inst))
  (block nil
    (map-rbnode3
      (lambda (node)
        (unless (check-red-rbtree3-node node)
          (return nil)))
      inst)
    t))

(defun check-red-rbtree3 (inst &key error)
  (declare (type rbtree3 inst))
  (let ((check (check-red-rbtree3-call inst)))
    (when (and error (null check))
      (error "check-red-rbtree3 error, ~S, ~A." inst check))
    check))

(defun check-red-rbtree3-error (inst)
  (declare (type rbtree3 inst))
  (check-red-rbtree3 inst :error t))


;;  compare
(defun check-compare-rbtree3-compare (node call)
  (let ((left (rbnode3-left node))
        (right (rbnode3-right node)))
    (when left
      (check-compare-rbtree3-compare left call))
    (funcall call node)
    (when right
      (check-compare-rbtree3-compare right call))))

(defun check-compare-rbtree3-call (inst)
  (block nil
    (let ((compare (rbtree3-compare inst))
          (root (rbtree3-root inst))
          x y)
      (when root
        (check-compare-rbtree3-compare
          root
          (lambda (node)
            (setq x y)
            (setq y (rbnode3-key node))
            (when x
              (unless (funcall compare x y)
                (return nil)))))))
    t))

(defun check-compare-rbtree3 (inst &key error)
  (declare (type rbtree3 inst))
  (let ((check (check-compare-rbtree3-call inst)))
    (when (and error (null check))
      (error "check-compare-rbtree3 error, ~S, ~A." inst check))
    check))

(defun check-compare-rbtree3-error (inst)
  (declare (type rbtree3 inst))
  (check-compare-rbtree3 inst :error t))


;;  length
(defun check-length-rbtree3-apply (node call &optional (count 0))
  (let* ((left (rbnode3-left node))
         (right (rbnode3-right node))
         (check1 (null left))
         (check2 (null right))
         (count1 (1+ count)))
    (when (and check1 check2)
      (funcall call count))
    (unless check1
      (check-length-rbtree3-apply left call count1))
    (unless check2
      (check-length-rbtree3-apply right call count1))))

(defun check-length-rbtree3-maxmin (inst)
  (let ((root (rbtree3-root inst))
        (max 0)
        (min 0))
    (check-length-rbtree3-apply
      root
      (lambda (count)
        (setq max (max max count))
        (setq min (min min count))))
    (values max min)))

(defun check-length-rbtree3-size (inst)
  (let ((size (rbtree3-size inst)))
    (if (eql size 0)
      (values size size)
      (check-length-rbtree3-maxmin inst))))

(defun check-length-rbtree3-call (inst)
  (multiple-value-bind (x y) (check-length-rbtree3-size inst)
    (values
      (or (and (zerop x) (zerop y))
          (and (eql x 1) (eql y 1))
          (<= y (* 2 x)))
      x y)))

(defun check-length-rbtree3 (inst &key error)
  (declare (type rbtree3 inst))
  (multiple-value-bind (check x y) (check-length-rbtree3-call inst)
    (when (and error (null check))
      (error "check-length-rbtree3 error, ~S, ~A, ~A, ~A." inst check x y))
    check))

(defun check-length-rbtree3-error (inst)
  (declare (type rbtree3 inst))
  (check-length-rbtree3 inst :error t))


;;  size
(defun check-size-rbtree3-map (inst)
  (declare (type rbtree3 inst))
  (let ((size 0))
    (map-rbnode3
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-rbtree3-call (inst)
  (declare (type rbtree3 inst))
  (let ((size (rbtree3-size inst))
        (check (check-size-rbtree3-map inst)))
    (values (= check size) check size)))

(defun check-size-rbtree3 (inst &key error)
  (declare (type rbtree3 inst))
  (multiple-value-bind (check count size) (check-size-rbtree3-call inst)
    (when (and error (null check))
      (error "check-size-rbtree3 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-rbtree3-error (inst)
  (declare (type rbtree3 inst))
  (check-size-rbtree3 inst :error t))


;;  check
(defun check-rbtree3 (inst &key error)
  (declare (type rbtree3 inst))
  (and (check-root-rbtree3 inst :error error)
       (check-parent-rbtree3 inst :error error)
       (check-red-rbtree3 inst :error error)
       (check-compare-rbtree3 inst :error error)
       (check-length-rbtree3 inst :error error)
       (check-size-rbtree3 inst :error error)
       t))

(defun check-rbtree3-error (inst)
  (declare (type rbtree3 inst))
  (check-rbtree3 inst :error t))

