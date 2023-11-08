;;
;;  AVL Tree
;;
(defpackage #:avltree1
  (:use common-lisp)
  (:export
    #:avltree1
    #:avltree1-p
    #:make-avltree1
    #:clear-avltree1
    #:size-avltree1
    #:avlnode1-p
    #:key-avlnode1
    #:value-avlnode1

    #:empty-avltree1
    #:min-avltree1
    #:min-avlnode1
    #:max-avltree1
    #:max-avlnode1
    #:map-avltree1
    #:map-avlnode1
    #:keys-avltree1
    #:values-avltree1
    #:hash-table-avltree1

    #:*replace-mode-avltree1*
    #:*replace-key-avltree1*
    #:insert-avltree1
    #:intern-avltree1
    #:search-avltree1
    #:search-avlnode1
    #:replace-avltree1
    #:delete-avltree1
    #:init-avltree1
    #:init-property-avltree1
    #:init-associate-avltree1))

(in-package #:avltree1)

(defvar *replace-mode-avltree1* nil)
(defvar *replace-key-avltree1* t)


;;
;;  avltree1
;;
(defstruct (avltree1
             (:constructor avltree1-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst avltree1) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avltree1-size inst))))

(defun make-avltree1 (&key (compare #'-))
  (avltree1-heap :compare compare))

(defun clear-avltree1 (inst)
  (declare (type avltree1 inst))
  (setf (avltree1-root inst) nil)
  (setf (avltree1-size inst) 0))

(defun size-avltree1 (inst)
  (declare (type avltree1 inst))
  (avltree1-size inst))


;;
;;  avlnode1
;;
(defstruct (avlnode1 (:copier nil))
  left right key value
  (balance 0 :type (integer -2 2)))  ;; left - right

(defmethod print-object ((inst avlnode1) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avlnode1-balance inst))))

(defun key-avlnode1 (inst)
  (declare (type avlnode1 inst))
  (avlnode1-key inst))

(defun value-avlnode1 (inst)
  (declare (type avlnode1 inst))
  (avlnode1-value inst))

(defun free-avlnode1 (delete)
  (setf (avlnode1-left delete) :error)
  (setf (avlnode1-right delete) :error)
  (setf (avlnode1-key delete) :error)
  (setf (avlnode1-value delete) :error)
  (setf (avlnode1-balance delete) 0)
  nil)


;;
;;  operator
;;
(defun empty-avltree1 (inst)
  (declare (type avltree1 inst))
  (if (avltree1-root inst) nil t))

;;  min
(defun min-node-avlnode1 (node)
  (let ((left (avlnode1-left node)))
    (if left
      (min-node-avlnode1 left)
      node)))

(defun min-avlnode1 (x)
  (etypecase x
    (avlnode1
      (min-node-avlnode1 x))
    (avltree1
      (setq x (avltree1-root x))
      (when x
        (min-node-avlnode1 x)))))

(defun min-avltree1 (inst)
  (declare (type avltree1 inst))
  (let ((node (min-avlnode1 inst)))
    (if node
      (values (avlnode1-key node) t)
      (values nil nil))))

;;  max
(defun max-node-avlnode1 (node)
  (let ((right (avlnode1-right node)))
    (if right
      (max-node-avlnode1 right)
      node)))

(defun max-avlnode1 (x)
  (etypecase x
    (avlnode1
      (max-node-avlnode1 x))
    (avltree1
      (setq x (avltree1-root x))
      (when x
        (max-node-avlnode1 x)))))

(defun max-avltree1 (inst)
  (declare (type avltree1 inst))
  (let ((node (max-avlnode1 inst)))
    (if node
      (values (avlnode1-key node) t)
      (values nil nil))))

;;  map
(defun map-avlnode1 (call inst)
  (declare (type avltree1 inst))
  (labels ((rec (x) (when x
                      (rec (avlnode1-right x))
                      (funcall call x)
                      (rec (avlnode1-left x)))))
    (rec (avltree1-root inst)))
  (values))

(defun map-avltree1 (call inst)
  (declare (type avltree1 inst))
  (map-avlnode1
    (lambda (x)
      (funcall call (avlnode1-key x) (avlnode1-value x)))
    inst))

(defun keys-avltree1 (inst)
  (declare (type avltree1 inst))
  (let (list)
    (map-avlnode1
      (lambda (x)
        (push (avlnode1-key x) list))
      inst)
    list))

(defun values-avltree1 (inst)
  (declare (type avltree1 inst))
  (let (list)
    (map-avlnode1
      (lambda (x)
        (push (avlnode1-value x) list))
      inst)
    list))

(defun hash-table-avltree1 (inst &optional (test 'eql))
  (declare (type avltree1 inst))
  (let ((table (make-hash-table :test test)))
    (map-avltree1
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))


;;
;;  rotate
;;
(defun rotate-left-avlnode1 (node)
  (let ((x (avlnode1-right node)))
    (setf (avlnode1-right node) (avlnode1-left x))
    (setf (avlnode1-left x) node)
    x))

(defun rotate-right-avlnode1 (node)
  (let ((x (avlnode1-left node)))
    (setf (avlnode1-left node) (avlnode1-right x))
    (setf (avlnode1-right x) node)
    x))


;;
;;  insert
;;
(defun replace-avlnode1 (node key value)
  (when *replace-key-avltree1*
    (setf (avlnode1-key node) key))
  (setf (avlnode1-value node) value))

(defun update-balance-avlnode1 (node)
  (let ((left (avlnode1-left node))
        (right (avlnode1-right node))
        (balance (avlnode1-balance node)))
    (cond ((= balance 1)
           (setf (avlnode1-balance left) 0)
           (setf (avlnode1-balance right) -1))
          ((= balance -1)
           (setf (avlnode1-balance left) 1)
           (setf (avlnode1-balance right) 0))
          (t (setf (avlnode1-balance left) 0)
             (setf (avlnode1-balance right) 0)))
    (setf (avlnode1-balance node) 0)
    node))

(defun insert-lr-avlnode1 (node left)
  (setf (avlnode1-left node) (rotate-left-avlnode1 left))
  (update-balance-avlnode1
    (rotate-right-avlnode1 node)))

(defun insert-ll-avlnode1 (node)
  (let ((next (rotate-right-avlnode1 node)))
    (setf (avlnode1-balance next) 0)
    (setf (avlnode1-balance node) 0)
    next))

(defun insert-left-avlnode1 (node)
  (let* ((left (avlnode1-left node))
         (balance (avlnode1-balance left)))
    (if (< balance 0)
      (insert-lr-avlnode1 node left)
      (insert-ll-avlnode1 node))))

(defun insert-rl-avlnode1 (node right)
  (setf (avlnode1-right node) (rotate-right-avlnode1 right))
  (update-balance-avlnode1
    (rotate-left-avlnode1 node)))

(defun insert-rr-avlnode1 (node)
  (let ((next (rotate-left-avlnode1 node)))
    (setf (avlnode1-balance next) 0)
    (setf (avlnode1-balance node) 0)
    next))

(defun insert-right-avlnode1 (node)
  (let* ((right (avlnode1-right node))
         (balance (avlnode1-balance right)))
    (if (< 0 balance)
      (insert-rl-avlnode1 node right)
      (insert-rr-avlnode1 node))))

(defun insert-avlnode1 (compare node key value)
  (declare (type (or avlnode1 null) node))
  (prog (key1 diff next check)
    ;;  nil
    (unless node
      (setq next (make-avlnode1 :key key :value value))
      (return (values next 'make)))

    ;;  move
    (setq key1 (avlnode1-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))

    ;;  equal
    (when *replace-mode-avltree1*
      (replace-avlnode1 node key value))
    (return (values nil nil))

    ;;  left
    move-left
    (setq next (avlnode1-left node))
    (multiple-value-setq (next check) (insert-avlnode1 compare next key value))
    (unless check
      (return (values nil nil)))
    (when (eq check t)
      (return (values node t)))
    (when (eq check 'finish)
      (setf (avlnode1-left node) next)
      (return (values node t)))
    (when (eq check 'make)
      (setf (avlnode1-left node) next))
    (incf (avlnode1-balance node) 1)
    (go balance)

    ;;  right
    move-right
    (setq next (avlnode1-right node))
    (multiple-value-setq (next check) (insert-avlnode1 compare next key value))
    (unless check
      (return (values nil nil)))
    (when (eq check t)
      (return (values node t)))
    (when (eq check 'finish)
      (setf (avlnode1-right node) next)
      (return (values node t)))
    (when (eq check 'make)
      (setf (avlnode1-right node) next))
    (decf (avlnode1-balance node) 1)
    (go balance)

    ;;  balance
    balance
    (setq check (avlnode1-balance node))
    (when (zerop check)
      (return (values node t)))  ;;  skip
    (when (< 1 check)  ;;  2
      (setq next (insert-left-avlnode1 node))
      (return (values next 'finish)))
    (when (< check -1)  ;;  -2
      (setq next (insert-right-avlnode1 node))
      (return (values next 'finish)))
    ;;  1 or -1
    (return (values node 'loop))))

(defun insert-avltree1
  (inst key value &optional (*replace-mode-avltree1* *replace-mode-avltree1*))
  (declare (type avltree1 inst))
  (let ((compare (avltree1-compare inst))
        (root (avltree1-root inst)))
    (multiple-value-bind (node check) (insert-avlnode1 compare root key value)
      (when check
        (setf (avltree1-root inst) node)
        (incf (avltree1-size inst) 1)
        t))))

(defun intern-avltree1 (inst key value)
  (declare (type avltree1 inst))
  (insert-avltree1 inst key value t))


;;
;;  search
;;
(defun search-avlnode1 (inst key)
  (declare (type avltree1 inst))
  (prog ((node (avltree1-root inst))
         (compare (avltree1-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (avlnode1-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (avlnode1-left node)))
          ((< 0 diff) (setq node (avlnode1-right node)))
          (t (return node)))
    (go loop)))

(defun search-avltree1 (inst key)
  (declare (type avltree1 inst))
  (let ((node (search-avlnode1 inst key)))
    (if node
      (values (avlnode1-value node) t)
      (values nil nil))))

(defun replace-avltree1
  (inst key value &optional (*replace-key-avltree1* *replace-key-avltree1*))
  (declare (type avltree1 inst))
  (let ((node (search-avlnode1 inst key)))
    (when node
      (replace-avlnode1 node key value)
      t)))


;;
;;  delete
;;
(defun delete-copy-avlnode1 (replace delete)
  (setf (avlnode1-key replace) (avlnode1-key delete))
  (setf (avlnode1-value replace) (avlnode1-value delete))
  (free-avlnode1 delete))

(defun delete-single-avlnode1 (node delete diff)
  (let* ((left (avlnode1-left delete))
         (right (avlnode1-right delete))
         (next (or left right)))
    (if (< diff 0)
      (setf (avlnode1-left node) next)
      (setf (avlnode1-right node) next))
    (free-avlnode1 delete)))

(defun delete-lr-avlnode1 (node left)
  (setf (avlnode1-left node) (rotate-left-avlnode1 left))
  (update-balance-avlnode1
    (rotate-right-avlnode1 node)))

(defun delete-ll-avlnode1 (node)
  (let ((next (rotate-right-avlnode1 node)))
    (if (zerop (avlnode1-balance next))
      (setf (avlnode1-balance next) -1
            (avlnode1-balance node) 1)
      (setf (avlnode1-balance next) 0
            (avlnode1-balance node) 0))
    next))

(defun delete-left-avlnode1 (node)
  (let* ((left (avlnode1-left node))
         (balance (avlnode1-balance left)))
    (if (< balance 0)
      (delete-lr-avlnode1 node left)
      (delete-ll-avlnode1 node))))

(defun delete-rl-avlnode1 (node right)
  (setf (avlnode1-right node) (rotate-right-avlnode1 right))
  (update-balance-avlnode1
    (rotate-left-avlnode1 node)))

(defun delete-rr-avlnode1 (node)
  (let ((next (rotate-left-avlnode1 node)))
    (if (zerop (avlnode1-balance next))
      (setf (avlnode1-balance next) 1
            (avlnode1-balance node) -1)
      (setf (avlnode1-balance next) 0
            (avlnode1-balance node) 0))
    next))

(defun delete-right-avlnode1 (node)
  (let* ((right (avlnode1-right node))
         (balance (avlnode1-balance right)))
    (if (< 0 balance)
      (delete-rl-avlnode1 node right)
      (delete-rr-avlnode1 node))))

(defun delete-balance-avlnode1 (node)
  (prog (next check)
    (setq check (avlnode1-balance node))
    (when (zerop check)  ;;  0
      (return (values nil 'loop)))
    (when (or (= check 1) (= check -1))  ;;  -1, 1
      (return (values nil t)))
    (when (< 1 check)  ;;  2
      (setq next (delete-left-avlnode1 node)))
    (when (< check -1)  ;;  -2
      (setq next (delete-right-avlnode1 node)))
    ;;  -2, 2
    (return (values next 'update))))

(defun delete-swap-avlnode1 (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  loop
    (setq next (avlnode1-left node))
    (multiple-value-setq (next check) (delete-swap-avlnode1 replace next))
    (unless check
      (return (values node 'replace)))
    (when (eq check t)
      (return (values nil t)))
    (when (eq check 'replace)
      (setf (avlnode1-left node) (avlnode1-right next))
      (delete-copy-avlnode1 replace next))
    (when (eq check 'update)
      (setf (avlnode1-left node) next)
      (unless (zerop (avlnode1-balance next))
        (return (values nil t))))

    ;;  loop, update
    (decf (avlnode1-balance node) 1)  ;; left
    (return (delete-balance-avlnode1 node))))

(defun delete-replace-avlnode1 (node next)
  (setf (avlnode1-right node) (avlnode1-right next))
  (delete-copy-avlnode1 node next)
  (incf (avlnode1-balance node) 1)  ;; right
  (delete-balance-avlnode1 node))

(defun delete-avlnode1 (compare node key)
  (prog (key1 diff left next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (avlnode1-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (setq next (avlnode1-left node))
      (go loop-move))
    (when (< 0 diff)
      (setq next (avlnode1-right node))
      (go loop-move))

    ;;  delete
    (setq left (avlnode1-left node))
    (setq next (avlnode1-right node))
    (unless (and left next)
      (return (values node 'delete)))
    (multiple-value-setq (next check) (delete-swap-avlnode1 node next))
    (when (eq check 'replace)
      (return (delete-replace-avlnode1 node next)))
    (go loop-result)

    ;;  loop
    loop-move
    (multiple-value-setq (next check) (delete-avlnode1 compare next key))
    (unless check
      (return (values nil nil)))

    loop-result
    (when (eq check t)
      (return (values nil t)))
    (when (eq check 'delete)
      (delete-single-avlnode1 node next diff))
    (when (eq check 'update)
      (if (< diff 0)
        (setf (avlnode1-left node) next)
        (setf (avlnode1-right node) next))
      (unless (zerop (avlnode1-balance next))
        (return (values nil t))))

    ;;  loop, delete, update
    (if (< diff 0)
      (decf (avlnode1-balance node) 1)
      (incf (avlnode1-balance node) 1))
    (return (delete-balance-avlnode1 node))))

(defun delete-setf-avltree1 (inst node)
  (setf (avltree1-root inst) node))

(defun delete-delete-avltree1 (inst delete)
  (let ((left (avlnode1-left delete))
        (right (avlnode1-right delete)))
    (setf (avltree1-root inst) (or left right))
    (free-avlnode1 delete)))

(defun delete-avltree1 (inst key)
  (declare (type avltree1 inst))
  (let ((compare (avltree1-compare inst))
        (root (avltree1-root inst)))
    (multiple-value-bind (node check) (delete-avlnode1 compare root key)
      (when check
        (ecase check
          ((t loop) nil)
          (delete (delete-delete-avltree1 inst node))
          (update (delete-setf-avltree1 inst node)))
        (decf (avltree1-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-avltree1-struct
  call depth limit lower direct)

(defun init-loop-avltree1 (str now)
  (prog* ((call (init-avltree1-struct-call str))
          (depth (init-avltree1-struct-depth str))
          (limit (init-avltree1-struct-limit str))
          (lower (init-avltree1-struct-lower str))
          (direct (init-avltree1-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-avltree1-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-avltree1 str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-avltree1 str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-avlnode1
                 :key key :value value :balance (- x y)
                 :left left :right right))
    (return (values node (max x y)))))

(defun init-call-avltree1 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-avltree1
      (make-init-avltree1-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-avltree1 (call size &optional inst direct)
  (unless inst
    (setq inst (make-avltree1)))
  (setf (avltree1-root inst) (init-call-avltree1 call size direct))
  (setf (avltree1-size inst) size)
  inst)

(defun init-property1-avltree1 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-avltree1 (seq size)
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

(defun init-property-avltree1 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-avltree1
        (if (listp seq)
          (init-property1-avltree1 seq)
          (init-property2-avltree1 seq div))
        div inst))))

(defun init-associate1-avltree1 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-avltree1 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-avltree1 (inst seq)
  (let ((size (length seq)))
    (init-avltree1
      (if (listp seq)
        (init-associate1-avltree1 seq)
        (init-associate2-avltree1 seq size))
      size inst)))

