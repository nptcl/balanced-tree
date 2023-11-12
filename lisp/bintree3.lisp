;;
;;  Binary Tree  (fixup)
;;
(defpackage #:bintree3
  (:use common-lisp)
  (:export
    #:bintree3
    #:bintree3-p
    #:make-bintree3
    #:clear-bintree3
    #:size-bintree3
    #:binnode3-p
    #:key-binnode3
    #:value-binnode3

    #:empty-bintree3
    #:min-bintree3
    #:min-binnode3
    #:max-bintree3
    #:max-binnode3
    #:map-bintree3
    #:map-binnode3
    #:keys-bintree3
    #:values-bintree3
    #:hash-table-bintree3
    #:next-binnode3
    #:prev-binnode3

    #:*replace-mode-bintree3*
    #:*replace-key-bintree3*
    #:insert-bintree3
    #:intern-bintree3
    #:search-bintree3
    #:search-binnode3
    #:replace-bintree3
    #:delete-bintree3
    #:init-bintree3
    #:init-property-bintree3
    #:init-associate-bintree3))

(in-package #:bintree3)

(defvar *replace-mode-bintree3* nil)
(defvar *replace-key-bintree3* t)


;;
;;  bintree3
;;
(defstruct (bintree3
             (:constructor bintree3-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst bintree3) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (bintree3-size inst))))

(defun make-bintree3 (&key (compare #'-))
  (bintree3-heap :compare compare))

(defun clear-bintree3 (inst)
  (declare (type bintree3 inst))
  (setf (bintree3-root inst) nil)
  (setf (bintree3-size inst) 0))

(defun size-bintree3 (inst)
  (declare (type bintree3 inst))
  (bintree3-size inst))


;;
;;  binnode3
;;
(defstruct (binnode3 (:copier nil))
  parent left right key value)

(defmethod print-object ((inst binnode3) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (binnode3-key inst))))

(defun key-binnode3 (inst)
  (declare (type binnode3 inst))
  (binnode3-key inst))

(defun value-binnode3 (inst)
  (declare (type binnode3 inst))
  (binnode3-value inst))

(defun free-binnode3 (delete)
  (setf (binnode3-parent delete) :error)
  (setf (binnode3-left delete) :error)
  (setf (binnode3-right delete) :error)
  (setf (binnode3-key delete) :error)
  (setf (binnode3-value delete) :error)
  nil)


;;
;;  operator
;;
(defun empty-bintree3 (inst)
  (declare (type bintree3 inst))
  (if (bintree3-root inst) nil t))

;;  min
(defun min-node-binnode3 (node)
  (let ((left (binnode3-left node)))
    (if left
      (min-node-binnode3 left)
      node)))

(defun min-binnode3 (x)
  (etypecase x
    (binnode3
      (min-node-binnode3 x))
    (bintree3
      (setq x (bintree3-root x))
      (when x
        (min-node-binnode3 x)))))

(defun min-bintree3 (inst)
  (declare (type bintree3 inst))
  (let ((node (min-binnode3 inst)))
    (if node
      (values (binnode3-key node) t)
      (values nil nil))))

;;  max
(defun max-node-binnode3 (node)
  (let ((right (binnode3-right node)))
    (if right
      (max-node-binnode3 right)
      node)))

(defun max-binnode3 (x)
  (etypecase x
    (binnode3
      (max-node-binnode3 x))
    (bintree3
      (setq x (bintree3-root x))
      (when x
        (max-node-binnode3 x)))))

(defun max-bintree3 (inst)
  (declare (type bintree3 inst))
  (let ((node (max-binnode3 inst)))
    (if node
      (values (binnode3-key node) t)
      (values nil nil))))

;;  map
(defun map-binnode3 (call inst)
  (declare (type bintree3 inst))
  (labels ((rec (x) (when x
                      (rec (binnode3-right x))
                      (funcall call x)
                      (rec (binnode3-left x)))))
    (rec (bintree3-root inst)))
  (values))

(defun map-bintree3 (call inst)
  (declare (type bintree3 inst))
  (map-binnode3
    (lambda (x)
      (funcall call (binnode3-key x) (binnode3-value x)))
    inst))

(defun keys-bintree3 (inst)
  (declare (type bintree3 inst))
  (let (list)
    (map-binnode3
      (lambda (x)
        (push (binnode3-key x) list))
      inst)
    list))

(defun values-bintree3 (inst)
  (declare (type bintree3 inst))
  (let (list)
    (map-binnode3
      (lambda (x)
        (push (binnode3-value x) list))
      inst)
    list))

(defun hash-table-bintree3 (inst &optional (test 'eql))
  (declare (type bintree3 inst))
  (let ((table (make-hash-table :test test)))
    (map-bintree3
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))

;;  parent
(defun next-binnode3 (x)
  (prog (y z)
    (setq y (binnode3-right x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (binnode3-left y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (binnode3-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (binnode3-right y))
    (unless (eq x z)
      (return y))
    (setq x y y (binnode3-parent y))
    (go parent-loop)))

(defun prev-binnode3 (x)
  (prog (y z)
    (setq y (binnode3-left x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (binnode3-right y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (binnode3-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (binnode3-left y))
    (unless (eq x z)
      (return y))
    (setq x y y (binnode3-parent y))
    (go parent-loop)))


;;
;;  insert fixup
;;
(defun insert-fixup-bintree3 (inst node)
  (declare (ignore inst node)))


;;
;;  insert
;;
(defun replace-binnode3 (node key value)
  (when *replace-key-bintree3*
    (setf (binnode3-key node) key))
  (setf (binnode3-value node) value))

(defun insert-binnode3 (compare node key value)
  (prog (key1 diff next)
    loop
    (setq key1 (binnode3-key node))
    (setq diff (funcall compare key key1))

    ;; left
    (when (< diff 0)
      (setq next (binnode3-left node))
      (unless next
        (setq next (make-binnode3 :key key :value value))
        (setf (binnode3-left node) next)
        (setf (binnode3-parent next) node)
        (return next))
      (setq node next)
      (go loop))

    ;; right
    (when (< 0 diff)
      (setq next (binnode3-right node))
      (unless next
        (setq next (make-binnode3 :key key :value value))
        (setf (binnode3-right node) next)
        (setf (binnode3-parent next) node)
        (return next))
      (setq node next)
      (go loop))

    ;;  equal
    (when *replace-mode-bintree3*
      (replace-binnode3 node key value))
    (return nil)))

(defun insert2-bintree3 (inst root key value)
  (let* ((compare (bintree3-compare inst))
         (node (insert-binnode3 compare root key value)))
    (when node
      (incf (bintree3-size inst) 1)
      node)))

(defun insert1-bintree3 (inst key value)
  (let ((node (make-binnode3 :key key :value value)))
    (setf (bintree3-root inst) node)
    (setf (bintree3-size inst) 1)
    node))

(defun insert0-bintree3 (inst key value)
  (let ((root (bintree3-root inst)))
    (if root
      (insert2-bintree3 inst root key value)
      (insert1-bintree3 inst key value))))

(defun insert-bintree3
  (inst key value &optional (*replace-mode-bintree3* *replace-mode-bintree3*))
  (let ((node (insert0-bintree3 inst key value)))
    (when node
      (insert-fixup-bintree3 inst node)
      t)))

(defun intern-bintree3 (inst key value)
  (declare (type bintree3 inst))
  (insert-bintree3 inst key value t))


;;
;;  search
;;
(defun search-binnode3 (inst key)
  (declare (type bintree3 inst))
  (prog ((node (bintree3-root inst))
         (compare (bintree3-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (binnode3-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (binnode3-left node)))
          ((< 0 diff) (setq node (binnode3-right node)))
          (t (return node)))
    (go loop)))

(defun search-bintree3 (inst key)
  (declare (type bintree3 inst))
  (let ((node (search-binnode3 inst key)))
    (if node
      (values (binnode3-value node) t)
      (values nil nil))))

(defun replace-bintree3
  (inst key value &optional (*replace-key-bintree3* *replace-key-bintree3*))
  (declare (type bintree3 inst))
  (let ((node (search-binnode3 inst key)))
    (when node
      (replace-binnode3 node key value)
      t)))


;;
;;  delete fixup
;;
(defun delete-fixup-bintree3 (inst node child direct)
  (declare (ignore inst node child direct)))


;;
;;  delete
;;
(defun delete-copy-binnode3 (replace delete)
  (setf (binnode3-key replace) (binnode3-key delete))
  (setf (binnode3-value replace) (binnode3-value delete)))

(defun delete-single-binnode3 (inst delete child)
  (let ((parent (binnode3-parent delete)) direct)
    (if parent
      (if (eq (binnode3-left parent) delete)
        (setf (binnode3-left parent) child direct 'left)
        (setf (binnode3-right parent) child direct 'right))
      (setf (bintree3-root inst) child direct nil))
    (when child
      (setf (binnode3-parent child) parent))
    (delete-fixup-bintree3 inst parent child direct)
    (free-binnode3 delete)))

(defun delete-swap-binnode3 (inst replace)
  (let* ((delete (next-binnode3 replace))
         (right (binnode3-right delete)))
    (delete-copy-binnode3 replace delete)
    (delete-single-binnode3 inst delete right)))

(defun delete-binnode3 (inst delete)
  (let ((left (binnode3-left delete))
        (right (binnode3-right delete)))
    (if (and left right)
      (delete-swap-binnode3 inst delete)
      (delete-single-binnode3 inst delete (or left right)))))

(defun delete-bintree3 (inst key)
  (declare (type bintree3 inst))
  (let ((delete (search-binnode3 inst key)))
    (when delete
      (delete-binnode3 inst delete)
      (decf (bintree3-size inst) 1)
      t)))


;;
;;  init
;;
(defstruct init-bintree3-struct
  call depth limit lower direct)

(defun init-loop-bintree3 (str now)
  (prog* ((call (init-bintree3-struct-call str))
          (depth (init-bintree3-struct-depth str))
          (limit (init-bintree3-struct-limit str))
          (lower (init-bintree3-struct-lower str))
          (direct (init-bintree3-struct-direct str))
          (depth1 (1- depth))
          node left right key value)
    (unless (< now depth)
      (return nil))
    (when (= now depth1)
      (unless (< lower limit)
        (return nil))
      (incf (init-bintree3-struct-lower str) 1))
    (incf now 1)
    (setq left (init-loop-bintree3 str now))
    (multiple-value-setq (key value) (funcall call))
    (setq right (init-loop-bintree3 str now))
    (when direct
      (rotatef left right))
    (setq node (make-binnode3 :key key :value value :left left :right right))
    (when left
      (setf (binnode3-parent left) node))
    (when right
      (setf (binnode3-parent right) node))
    (return node)))

(defun init-call-bintree3 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-bintree3
      (make-init-bintree3-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-bintree3 (call size &optional inst direct)
  (unless inst
    (setq inst (make-bintree3)))
  (setf (bintree3-root inst) (init-call-bintree3 call size direct))
  (setf (bintree3-size inst) size)
  inst)

(defun init-property1-bintree3 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-bintree3 (seq size)
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

(defun init-property-bintree3 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-bintree3
        (if (listp seq)
          (init-property1-bintree3 seq)
          (init-property2-bintree3 seq div))
        div inst))))

(defun init-associate1-bintree3 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-bintree3 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-bintree3 (inst seq)
  (let ((size (length seq)))
    (init-bintree3
      (if (listp seq)
        (init-associate1-bintree3 seq)
        (init-associate2-bintree3 seq size))
      size inst)))

