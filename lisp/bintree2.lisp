;;
;;  Binary Tree  (parent link)
;;
(defpackage #:bintree2
  (:use common-lisp)
  (:export
    #:bintree2
    #:bintree2-p
    #:make-bintree2
    #:clear-bintree2
    #:size-bintree2
    #:binnode2-p
    #:key-binnode2
    #:value-binnode2

    #:empty-bintree2
    #:min-bintree2
    #:min-binnode2
    #:max-bintree2
    #:max-binnode2
    #:map-bintree2
    #:map-binnode2
    #:keys-bintree2
    #:values-bintree2
    #:hash-table-bintree2
    #:next-binnode2
    #:prev-binnode2

    #:*replace-mode-bintree2*
    #:*replace-key-bintree2*
    #:insert-bintree2
    #:intern-bintree2
    #:search-bintree2
    #:search-binnode2
    #:replace-bintree2
    #:delete-bintree2
    #:init-bintree2
    #:init-property-bintree2
    #:init-associate-bintree2))

(in-package #:bintree2)

(defvar *replace-mode-bintree2* nil)
(defvar *replace-key-bintree2* t)


;;
;;  bintree2
;;
(defstruct (bintree2
             (:constructor bintree2-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst bintree2) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (bintree2-size inst))))

(defun make-bintree2 (&key (compare #'-))
  (bintree2-heap :compare compare))

(defun clear-bintree2 (inst)
  (declare (type bintree2 inst))
  (setf (bintree2-root inst) nil)
  (setf (bintree2-size inst) 0))

(defun size-bintree2 (inst)
  (declare (type bintree2 inst))
  (bintree2-size inst))


;;
;;  binnode2
;;
(defstruct (binnode2 (:copier nil))
  parent left right key value)

(defmethod print-object ((inst binnode2) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (binnode2-key inst))))

(defun key-binnode2 (inst)
  (declare (type binnode2 inst))
  (binnode2-key inst))

(defun value-binnode2 (inst)
  (declare (type binnode2 inst))
  (binnode2-value inst))

(defun free-binnode2 (delete)
  (setf (binnode2-parent delete) :error)
  (setf (binnode2-left delete) :error)
  (setf (binnode2-right delete) :error)
  (setf (binnode2-key delete) :error)
  (setf (binnode2-value delete) :error)
  nil)


;;
;;  operator
;;
(defun empty-bintree2 (inst)
  (declare (type bintree2 inst))
  (if (bintree2-root inst) nil t))

;;  min
(defun min-node-binnode2 (node)
  (let ((left (binnode2-left node)))
    (if left
      (min-node-binnode2 left)
      node)))

(defun min-binnode2 (x)
  (etypecase x
    (binnode2
      (min-node-binnode2 x))
    (bintree2
      (setq x (bintree2-root x))
      (when x
        (min-node-binnode2 x)))))

(defun min-bintree2 (inst)
  (declare (type bintree2 inst))
  (let ((node (min-binnode2 inst)))
    (if node
      (values (binnode2-key node) t)
      (values nil nil))))

;;  max
(defun max-node-binnode2 (node)
  (let ((right (binnode2-right node)))
    (if right
      (max-node-binnode2 right)
      node)))

(defun max-binnode2 (x)
  (etypecase x
    (binnode2
      (max-node-binnode2 x))
    (bintree2
      (setq x (bintree2-root x))
      (when x
        (max-node-binnode2 x)))))

(defun max-bintree2 (inst)
  (declare (type bintree2 inst))
  (let ((node (max-binnode2 inst)))
    (if node
      (values (binnode2-key node) t)
      (values nil nil))))

;;  map
(defun map-binnode2 (call inst)
  (declare (type bintree2 inst))
  (labels ((rec (x) (when x
                      (rec (binnode2-right x))
                      (funcall call x)
                      (rec (binnode2-left x)))))
    (rec (bintree2-root inst)))
  (values))

(defun map-bintree2 (call inst)
  (declare (type bintree2 inst))
  (map-binnode2
    (lambda (x)
      (funcall call (binnode2-key x) (binnode2-value x)))
    inst))

(defun keys-bintree2 (inst)
  (declare (type bintree2 inst))
  (let (list)
    (map-binnode2
      (lambda (x)
        (push (binnode2-key x) list))
      inst)
    list))

(defun values-bintree2 (inst)
  (declare (type bintree2 inst))
  (let (list)
    (map-binnode2
      (lambda (x)
        (push (binnode2-value x) list))
      inst)
    list))

(defun hash-table-bintree2 (inst &optional (test 'eql))
  (declare (type bintree2 inst))
  (let ((table (make-hash-table :test test)))
    (map-bintree2
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))

;;  parent
(defun next-binnode2 (x)
  (prog (y z)
    (setq y (binnode2-right x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (binnode2-left y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (binnode2-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (binnode2-right y))
    (unless (eq x z)
      (return y))
    (setq x y y (binnode2-parent y))
    (go parent-loop)))

(defun prev-binnode2 (x)
  (prog (y z)
    (setq y (binnode2-left x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (binnode2-right y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (binnode2-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (binnode2-left y))
    (unless (eq x z)
      (return y))
    (setq x y y (binnode2-parent y))
    (go parent-loop)))


;;
;;  insert
;;
(defun replace-binnode2 (node key value)
  (when *replace-key-bintree2*
    (setf (binnode2-key node) key))
  (setf (binnode2-value node) value))

(defun insert-binnode2 (compare node key value)
  (prog (key1 diff next)
    loop
    (setq key1 (binnode2-key node))
    (setq diff (funcall compare key key1))

    ;; left
    (when (< diff 0)
      (setq next (binnode2-left node))
      (unless next
        (setq next (make-binnode2 :key key :value value))
        (setf (binnode2-left node) next)
        (setf (binnode2-parent next) node)
        (return t))
      (setq node next)
      (go loop))

    ;; right
    (when (< 0 diff)
      (setq next (binnode2-right node))
      (unless next
        (setq next (make-binnode2 :key key :value value))
        (setf (binnode2-right node) next)
        (setf (binnode2-parent next) node)
        (return t))
      (setq node next)
      (go loop))

    ;;  equal
    (when *replace-mode-bintree2*
      (replace-binnode2 node key value))
    (return nil)))

(defun insert2-bintree2 (inst root key value)
  (let ((compare (bintree2-compare inst)))
    (when (insert-binnode2 compare root key value)
      (incf (bintree2-size inst) 1)
      t)))

(defun insert1-bintree2 (inst key value)
  (let ((node (make-binnode2 :key key :value value)))
    (setf (bintree2-root inst) node)
    (setf (bintree2-size inst) 1)
    t))

(defun insert-bintree2
  (inst key value &optional (*replace-mode-bintree2* *replace-mode-bintree2*))
  (let ((root (bintree2-root inst)))
    (if root
      (insert2-bintree2 inst root key value)
      (insert1-bintree2 inst key value))))

(defun intern-bintree2 (inst key value)
  (declare (type bintree2 inst))
  (insert-bintree2 inst key value t))


;;
;;  search
;;
(defun search-binnode2 (inst key)
  (declare (type bintree2 inst))
  (prog ((node (bintree2-root inst))
         (compare (bintree2-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (binnode2-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (binnode2-left node)))
          ((< 0 diff) (setq node (binnode2-right node)))
          (t (return node)))
    (go loop)))

(defun search-bintree2 (inst key)
  (declare (type bintree2 inst))
  (let ((node (search-binnode2 inst key)))
    (if node
      (values (binnode2-value node) t)
      (values nil nil))))

(defun replace-bintree2
  (inst key value &optional (*replace-key-bintree2* *replace-key-bintree2*))
  (declare (type bintree2 inst))
  (let ((node (search-binnode2 inst key)))
    (when node
      (replace-binnode2 node key value)
      t)))


;;
;;  delete
;;
(defun delete-copy-binnode2 (replace delete)
  (setf (binnode2-key replace) (binnode2-key delete))
  (setf (binnode2-value replace) (binnode2-value delete))
  (free-binnode2 delete))

(defun delete-swap-binnode2 (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  left
    (setq next (binnode2-left node))
    (multiple-value-setq (next check) (delete-swap-binnode2 replace next))
    (when check
      (setf (binnode2-left node) next)
      (when next
        (setf (binnode2-parent next) node))
      (return (values node t)))

    ;;  right
    (setq next (binnode2-right node))
    (delete-copy-binnode2 replace node)
    (return (values next t))))

(defun delete-binnode2 (compare node key)
  (prog (key1 diff left right next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (binnode2-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))
    (go move-delete)

    ;;  left
    move-left
    (setq next (binnode2-left node))
    (multiple-value-setq (next check) (delete-binnode2 compare next key))
    (unless check
      (return (values nil nil)))
    (setf (binnode2-left node) next)
    (when next
      (setf (binnode2-parent next) node))
    (return (values node t))

    ;;  right
    move-right
    (setq next (binnode2-right node))
    (multiple-value-setq (next check) (delete-binnode2 compare next key))
    (unless check
      (return (values nil nil)))
    (setf (binnode2-right node) next)
    (when next
      (setf (binnode2-parent next) node))
    (return (values node t))

    ;;  delete
    move-delete
    (setq left (binnode2-left node))
    (setq right (binnode2-right node))
    (when (and left right)
      (go delete-swap))
    (when left
      (go delete-left))
    (when right
      (go delete-right))
    (go delete-single)

    ;;  left, right
    delete-swap
    (multiple-value-setq (next check) (delete-swap-binnode2 node right))
    (setf (binnode2-right node) next)
    (when next
      (setf (binnode2-parent next) node))
    (return (values node t))

    ;;  left only
    delete-left
    (free-binnode2 node)
    (return (values left t))

    ;;  right only
    delete-right
    (free-binnode2 node)
    (return (values right t))

    ;;  null, null
    delete-single
    (free-binnode2 node)
    (return (values nil t))))

(defun delete-bintree2 (inst key)
  (declare (type bintree2 inst))
  (let ((compare (bintree2-compare inst))
        (root (bintree2-root inst)))
    (multiple-value-bind (node check) (delete-binnode2 compare root key)
      (when check
        (setf (bintree2-root inst) node)
        (when node
          (setf (binnode2-parent node) nil))
        (decf (bintree2-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-bintree2-struct
  call depth limit lower direct)

(defun init-loop-bintree2 (str now)
  (prog* ((call (init-bintree2-struct-call str))
          (depth (init-bintree2-struct-depth str))
          (limit (init-bintree2-struct-limit str))
          (lower (init-bintree2-struct-lower str))
          (direct (init-bintree2-struct-direct str))
          (depth1 (1- depth))
          node left right key value)
    (unless (< now depth)
      (return nil))
    (when (= now depth1)
      (unless (< lower limit)
        (return nil))
      (incf (init-bintree2-struct-lower str) 1))
    (incf now 1)
    (setq left (init-loop-bintree2 str now))
    (multiple-value-setq (key value) (funcall call))
    (setq right (init-loop-bintree2 str now))
    (when direct
      (rotatef left right))
    (setq node (make-binnode2 :key key :value value :left left :right right))
    (when left
      (setf (binnode2-parent left) node))
    (when right
      (setf (binnode2-parent right) node))
    (return node)))

(defun init-call-bintree2 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-bintree2
      (make-init-bintree2-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-bintree2 (call size &optional inst direct)
  (unless inst
    (setq inst (make-bintree2)))
  (setf (bintree2-root inst) (init-call-bintree2 call size direct))
  (setf (bintree2-size inst) size)
  inst)

(defun init-property1-bintree2 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-bintree2 (seq size)
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

(defun init-property-bintree2 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-bintree2
        (if (listp seq)
          (init-property1-bintree2 seq)
          (init-property2-bintree2 seq div))
        div inst))))

(defun init-associate1-bintree2 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-bintree2 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-bintree2 (inst seq)
  (let ((size (length seq)))
    (init-bintree2
      (if (listp seq)
        (init-associate1-bintree2 seq)
        (init-associate2-bintree2 seq size))
      size inst)))

