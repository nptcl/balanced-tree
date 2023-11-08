;;
;;  Binary Tree
;;
(defpackage #:bintree1
  (:use common-lisp)
  (:export
    #:bintree1
    #:bintree1-p
    #:make-bintree1
    #:clear-bintree1
    #:size-bintree1
    #:binnode1-p
    #:key-binnode1
    #:value-binnode1

    #:empty-bintree1
    #:min-bintree1
    #:min-binnode1
    #:max-bintree1
    #:max-binnode1
    #:map-bintree1
    #:map-binnode1
    #:keys-bintree1
    #:values-bintree1
    #:hash-table-bintree1

    #:*replace-mode-bintree1*
    #:*replace-key-bintree1*
    #:insert-bintree1
    #:intern-bintree1
    #:search-bintree1
    #:search-binnode1
    #:replace-bintree1
    #:delete-bintree1
    #:init-bintree1
    #:init-property-bintree1
    #:init-associate-bintree1))

(in-package #:bintree1)

(defvar *replace-mode-bintree1* nil)
(defvar *replace-key-bintree1* t)


;;
;;  bintree1
;;
(defstruct (bintree1
             (:constructor bintree1-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst bintree1) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (bintree1-size inst))))

(defun make-bintree1 (&key (compare #'-))
  (bintree1-heap :compare compare))

(defun clear-bintree1 (inst)
  (declare (type bintree1 inst))
  (setf (bintree1-root inst) nil)
  (setf (bintree1-size inst) 0))

(defun size-bintree1 (inst)
  (declare (type bintree1 inst))
  (bintree1-size inst))


;;
;;  binnode1
;;
(defstruct (binnode1 (:copier nil))
  left right key value)

(defmethod print-object ((inst binnode1) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (binnode1-key inst))))

(defun key-binnode1 (inst)
  (declare (type binnode1 inst))
  (binnode1-key inst))

(defun value-binnode1 (inst)
  (declare (type binnode1 inst))
  (binnode1-value inst))

(defun free-binnode1 (delete)
  (setf (binnode1-left delete) :error)
  (setf (binnode1-right delete) :error)
  (setf (binnode1-key delete) :error)
  (setf (binnode1-value delete) :error)
  nil)


;;
;;  operator
;;
(defun empty-bintree1 (inst)
  (declare (type bintree1 inst))
  (if (bintree1-root inst) nil t))

;;  min
(defun min-node-binnode1 (node)
  (let ((left (binnode1-left node)))
    (if left
      (min-node-binnode1 left)
      node)))

(defun min-binnode1 (x)
  (etypecase x
    (binnode1
      (min-node-binnode1 x))
    (bintree1
      (setq x (bintree1-root x))
      (when x
        (min-node-binnode1 x)))))

(defun min-bintree1 (inst)
  (declare (type bintree1 inst))
  (let ((node (min-binnode1 inst)))
    (if node
      (values (binnode1-key node) t)
      (values nil nil))))

;;  max
(defun max-node-binnode1 (node)
  (let ((right (binnode1-right node)))
    (if right
      (max-node-binnode1 right)
      node)))

(defun max-binnode1 (x)
  (etypecase x
    (binnode1
      (max-node-binnode1 x))
    (bintree1
      (setq x (bintree1-root x))
      (when x
        (max-node-binnode1 x)))))

(defun max-bintree1 (inst)
  (declare (type bintree1 inst))
  (let ((node (max-binnode1 inst)))
    (if node
      (values (binnode1-key node) t)
      (values nil nil))))

;;  map
(defun map-binnode1 (call inst)
  (declare (type bintree1 inst))
  (labels ((rec (x) (when x
                      (rec (binnode1-right x))
                      (funcall call x)
                      (rec (binnode1-left x)))))
    (rec (bintree1-root inst)))
  (values))

(defun map-bintree1 (call inst)
  (declare (type bintree1 inst))
  (map-binnode1
    (lambda (x)
      (funcall call (binnode1-key x) (binnode1-value x)))
    inst))

(defun keys-bintree1 (inst)
  (declare (type bintree1 inst))
  (let (list)
    (map-binnode1
      (lambda (x)
        (push (binnode1-key x) list))
      inst)
    list))

(defun values-bintree1 (inst)
  (declare (type bintree1 inst))
  (let (list)
    (map-binnode1
      (lambda (x)
        (push (binnode1-value x) list))
      inst)
    list))

(defun hash-table-bintree1 (inst &optional (test 'eql))
  (declare (type bintree1 inst))
  (let ((table (make-hash-table :test test)))
    (map-bintree1
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))


;;
;;  insert
;;
(defun replace-binnode1 (node key value)
  (when *replace-key-bintree1*
    (setf (binnode1-key node) key))
  (setf (binnode1-value node) value))

(defun insert-binnode1 (compare node key value)
  (prog (key1 diff next)
    loop
    (setq key1 (binnode1-key node))
    (setq diff (funcall compare key key1))

    ;; left
    (when (< diff 0)
      (setq next (binnode1-left node))
      (unless next
        (setf (binnode1-left node) (make-binnode1 :key key :value value))
        (return t))
      (setq node next)
      (go loop))

    ;; right
    (when (< 0 diff)
      (setq next (binnode1-right node))
      (unless next
        (setf (binnode1-right node) (make-binnode1 :key key :value value))
        (return t))
      (setq node next)
      (go loop))

    ;;  equal
    (when *replace-mode-bintree1*
      (replace-binnode1 node key value))
    (return nil)))

(defun insert2-bintree1 (inst root key value)
  (let ((compare (bintree1-compare inst)))
    (when (insert-binnode1 compare root key value)
      (incf (bintree1-size inst) 1)
      t)))

(defun insert1-bintree1 (inst key value)
  (let ((node (make-binnode1 :key key :value value)))
    (setf (bintree1-root inst) node)
    (setf (bintree1-size inst) 1)
    t))

(defun insert-bintree1
  (inst key value &optional (*replace-mode-bintree1* *replace-mode-bintree1*))
  (let ((root (bintree1-root inst)))
    (if root
      (insert2-bintree1 inst root key value)
      (insert1-bintree1 inst key value))))

(defun intern-bintree1 (inst key value)
  (declare (type bintree1 inst))
  (insert-bintree1 inst key value t))


;;
;;  search
;;
(defun search-binnode1 (inst key)
  (declare (type bintree1 inst))
  (prog ((node (bintree1-root inst))
         (compare (bintree1-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (binnode1-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (binnode1-left node)))
          ((< 0 diff) (setq node (binnode1-right node)))
          (t (return node)))
    (go loop)))

(defun search-bintree1 (inst key)
  (declare (type bintree1 inst))
  (let ((node (search-binnode1 inst key)))
    (if node
      (values (binnode1-value node) t)
      (values nil nil))))

(defun replace-bintree1
  (inst key value &optional (*replace-key-bintree1* *replace-key-bintree1*))
  (declare (type bintree1 inst))
  (let ((node (search-binnode1 inst key)))
    (when node
      (replace-binnode1 node key value)
      t)))


;;
;;  delete
;;
(defun delete-copy-binnode1 (replace delete)
  (setf (binnode1-key replace) (binnode1-key delete))
  (setf (binnode1-value replace) (binnode1-value delete))
  (free-binnode1 delete))

(defun delete-swap-binnode1 (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  left
    (setq next (binnode1-left node))
    (multiple-value-setq (next check) (delete-swap-binnode1 replace next))
    (when check
      (setf (binnode1-left node) next)
      (return (values node t)))

    ;;  right
    (setq next (binnode1-right node))
    (delete-copy-binnode1 replace node)
    (return (values next t))))

(defun delete-binnode1 (compare node key)
  (prog (key1 diff left right next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (binnode1-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))
    (go move-delete)

    ;;  left
    move-left
    (setq next (binnode1-left node))
    (multiple-value-setq (next check) (delete-binnode1 compare next key))
    (unless check
      (return (values nil nil)))
    (setf (binnode1-left node) next)
    (return (values node t))

    ;;  right
    move-right
    (setq next (binnode1-right node))
    (multiple-value-setq (next check) (delete-binnode1 compare next key))
    (unless check
      (return (values nil nil)))
    (setf (binnode1-right node) next)
    (return (values node t))

    ;;  delete
    move-delete
    (setq left (binnode1-left node))
    (setq right (binnode1-right node))
    (when (and left right)
      (go delete-swap))
    (when left
      (go delete-left))
    (when right
      (go delete-right))
    (go delete-single)

    ;;  left, right
    delete-swap
    (multiple-value-setq (next check) (delete-swap-binnode1 node right))
    (setf (binnode1-right node) next)
    (return (values node t))

    ;;  left only
    delete-left
    (free-binnode1 node)
    (return (values left t))

    ;;  right only
    delete-right
    (free-binnode1 node)
    (return (values right t))

    ;;  null, null
    delete-single
    (free-binnode1 node)
    (return (values nil t))))

(defun delete-bintree1 (inst key)
  (declare (type bintree1 inst))
  (let ((compare (bintree1-compare inst))
        (root (bintree1-root inst)))
    (multiple-value-bind (node check) (delete-binnode1 compare root key)
      (when check
        (setf (bintree1-root inst) node)
        (decf (bintree1-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-bintree1-struct
  call depth limit lower direct)

(defun init-loop-bintree1 (str now)
  (prog* ((call (init-bintree1-struct-call str))
          (depth (init-bintree1-struct-depth str))
          (limit (init-bintree1-struct-limit str))
          (lower (init-bintree1-struct-lower str))
          (direct (init-bintree1-struct-direct str))
          (depth1 (1- depth))
          node left right key value)
    (unless (< now depth)
      (return nil))
    (when (= now depth1)
      (unless (< lower limit)
        (return nil))
      (incf (init-bintree1-struct-lower str) 1))
    (incf now 1)
    (setq left (init-loop-bintree1 str now))
    (multiple-value-setq (key value) (funcall call))
    (setq right (init-loop-bintree1 str now))
    (when direct
      (rotatef left right))
    (setq node (make-binnode1 :key key :value value :left left :right right))
    (return node)))

(defun init-call-bintree1 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-bintree1
      (make-init-bintree1-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-bintree1 (call size &optional inst direct)
  (unless inst
    (setq inst (make-bintree1)))
  (setf (bintree1-root inst) (init-call-bintree1 call size direct))
  (setf (bintree1-size inst) size)
  inst)

(defun init-property1-bintree1 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-bintree1 (seq size)
  (let ((index 0))
    (lambda (&aux key value)
      (unless (< index size)
        (error "sequence error, key."))
      (setq key (aref seq index))
      (incf index 1)
      (unless (< index size)
        (error "sequence error, value, ~S." key))
      (setq value (aref seq index))
      (incf index 1)
      (values key value))))

(defun init-property-bintree1 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-bintree1
        (if (listp seq)
          (init-property1-bintree1 seq)
          (init-property2-bintree1 seq div))
        div inst))))

(defun init-associate1-bintree1 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-bintree1 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-bintree1 (inst seq)
  (let ((size (length seq)))
    (init-bintree1
      (if (listp seq)
        (init-associate1-bintree1 seq)
        (init-associate2-bintree1 seq size))
      size inst)))

