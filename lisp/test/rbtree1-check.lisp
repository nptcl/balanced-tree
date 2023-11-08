(in-package #:rbtree1)

(export 'check-rbtree1)
(export 'check-rbtree1-error)

;;  root
(defun check-root-rbtree1-call (inst)
  (declare (type rbtree1 inst))
  (let ((root (rbtree1-root inst)))
    (or (and (null root)
             (eql (rbtree1-size inst) 0))
        (eq (rbnode1-color root) 'black))))

(defun check-root-rbtree1 (inst &key error)
  (declare (type rbtree1 inst))
  (let ((check (check-root-rbtree1-call inst)))
    (when (and error (null check))
      (error "check-root-rbtree1 error, ~S, ~A." inst check))
    check))

(defun check-root-rbtree1-error (inst)
  (declare (type rbtree1 inst))
  (check-root-rbtree1 inst :error t))


;;  red node
(defun check-red-rbtree1-node (node)
  (let ((left (rbnode1-left node))
        (right (rbnode1-right node))
        (color (rbnode1-color node)))
    (or (eq color 'black)
        (and (eq color 'red)
             (or (null left) (eq (rbnode1-color left) 'black))
             (or (null right) (eq (rbnode1-color right) 'black))))))

(defun check-red-rbtree1-call (inst)
  (declare (type rbtree1 inst))
  (block nil
    (map-rbnode1
      (lambda (node)
        (unless (check-red-rbtree1-node node)
          (return nil)))
      inst)
    t))

(defun check-red-rbtree1 (inst &key error)
  (declare (type rbtree1 inst))
  (let ((check (check-red-rbtree1-call inst)))
    (when (and error (null check))
      (error "check-red-rbtree1 error, ~S, ~A." inst check))
    check))

(defun check-red-rbtree1-error (inst)
  (declare (type rbtree1 inst))
  (check-red-rbtree1 inst :error t))


;;  compare
(defun check-compare-rbtree1-compare (node call)
  (let ((left (rbnode1-left node))
        (right (rbnode1-right node)))
    (when left
      (check-compare-rbtree1-compare left call))
    (funcall call node)
    (when right
      (check-compare-rbtree1-compare right call))))

(defun check-compare-rbtree1-call (inst)
  (block nil
    (let ((compare (rbtree1-compare inst))
          (root (rbtree1-root inst))
          x y)
      (when root
        (check-compare-rbtree1-compare
          root
          (lambda (node)
            (setq x y)
            (setq y (rbnode1-key node))
            (when x
              (unless (funcall compare x y)
                (return nil)))))))
    t))

(defun check-compare-rbtree1 (inst &key error)
  (declare (type rbtree1 inst))
  (let ((check (check-compare-rbtree1-call inst)))
    (when (and error (null check))
      (error "check-compare-rbtree1 error, ~S, ~A." inst check))
    check))

(defun check-compare-rbtree1-error (inst)
  (declare (type rbtree1 inst))
  (check-compare-rbtree1 inst :error t))


;;  length
(defun check-length-rbtree1-apply (node call &optional (count 0))
  (let* ((left (rbnode1-left node))
         (right (rbnode1-right node))
         (check1 (null left))
         (check2 (null right))
         (count1 (1+ count)))
    (when (and check1 check2)
      (funcall call count))
    (unless check1
      (check-length-rbtree1-apply left call count1))
    (unless check2
      (check-length-rbtree1-apply right call count1))))

(defun check-length-rbtree1-maxmin (inst)
  (let ((root (rbtree1-root inst))
        (max 0)
        (min 0))
    (check-length-rbtree1-apply
      root
      (lambda (count)
        (setq max (max max count))
        (setq min (min min count))))
    (values max min)))

(defun check-length-rbtree1-size (inst)
  (let ((size (rbtree1-size inst)))
    (if (eql size 0)
      (values size size)
      (check-length-rbtree1-maxmin inst))))

(defun check-length-rbtree1-call (inst)
  (multiple-value-bind (x y) (check-length-rbtree1-size inst)
    (values
      (or (and (zerop x) (zerop y))
          (and (eql x 1) (eql y 1))
          (<= y (* 2 x)))
      x y)))

(defun check-length-rbtree1 (inst &key error)
  (declare (type rbtree1 inst))
  (multiple-value-bind (check x y) (check-length-rbtree1-call inst)
    (when (and error (null check))
      (error "check-length-rbtree1 error, ~S, ~A, ~A, ~A." inst check x y))
    check))

(defun check-length-rbtree1-error (inst)
  (declare (type rbtree1 inst))
  (check-length-rbtree1 inst :error t))


;;  size
(defun check-size-rbtree1-map (inst)
  (declare (type rbtree1 inst))
  (let ((size 0))
    (map-rbnode1
      (lambda (x)
        (declare (ignore x))
        (incf size 1))
      inst)
    size))

(defun check-size-rbtree1-call (inst)
  (declare (type rbtree1 inst))
  (let ((size (rbtree1-size inst))
        (check (check-size-rbtree1-map inst)))
    (values (= check size) check size)))

(defun check-size-rbtree1 (inst &key error)
  (declare (type rbtree1 inst))
  (multiple-value-bind (check count size) (check-size-rbtree1-call inst)
    (when (and error (null check))
      (error "check-size-rbtree1 error, ~S, ~A /= ~A." inst count size))
    (values check count size)))

(defun check-size-rbtree1-error (inst)
  (declare (type rbtree1 inst))
  (check-size-rbtree1 inst :error t))


;;  check
(defun check-rbtree1 (inst &key error)
  (declare (type rbtree1 inst))
  (and (check-root-rbtree1 inst :error error)
       (check-red-rbtree1 inst :error error)
       (check-compare-rbtree1 inst :error error)
       (check-length-rbtree1 inst :error error)
       (check-size-rbtree1 inst :error error)
       t))

(defun check-rbtree1-error (inst)
  (declare (type rbtree1 inst))
  (check-rbtree1 inst :error t))

