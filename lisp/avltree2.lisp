;;
;;  AVL Tree  (parent)
;;
(defpackage #:avltree2
  (:use common-lisp)
  (:export
    #:avltree2
    #:avltree2-p
    #:make-avltree2
    #:clear-avltree2
    #:size-avltree2
    #:avlnode2-p
    #:key-avlnode2
    #:value-avlnode2

    #:empty-avltree2
    #:min-avltree2
    #:min-avlnode2
    #:max-avltree2
    #:max-avlnode2
    #:map-avltree2
    #:map-avlnode2
    #:keys-avltree2
    #:values-avltree2
    #:hash-table-avltree2
    #:next-avlnode2
    #:prev-avlnode2

    #:*replace-mode-avltree2*
    #:*replace-key-avltree2*
    #:insert-avltree2
    #:intern-avltree2
    #:search-avltree2
    #:search-avlnode2
    #:replace-avltree2
    #:delete-avltree2
    #:init-avltree2
    #:init-property-avltree2
    #:init-associate-avltree2))

(in-package #:avltree2)

(defvar *replace-mode-avltree2* nil)
(defvar *replace-key-avltree2* t)


;;
;;  avltree2
;;
(defstruct (avltree2
             (:constructor avltree2-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst avltree2) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avltree2-size inst))))

(defun make-avltree2 (&key (compare #'-))
  (avltree2-heap :compare compare))

(defun clear-avltree2 (inst)
  (declare (type avltree2 inst))
  (setf (avltree2-root inst) nil)
  (setf (avltree2-size inst) 0))

(defun size-avltree2 (inst)
  (declare (type avltree2 inst))
  (avltree2-size inst))


;;
;;  avlnode2
;;
(defstruct (avlnode2 (:copier nil))
  parent left right key value
  (balance 0 :type (integer -2 2)))  ;; left - right

(defmethod print-object ((inst avlnode2) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avlnode2-balance inst))))

(defun key-avlnode2 (inst)
  (declare (type avlnode2 inst))
  (avlnode2-key inst))

(defun value-avlnode2 (inst)
  (declare (type avlnode2 inst))
  (avlnode2-value inst))

(defun free-avlnode2 (delete)
  (setf (avlnode2-parent delete) :error)
  (setf (avlnode2-left delete) :error)
  (setf (avlnode2-right delete) :error)
  (setf (avlnode2-key delete) :error)
  (setf (avlnode2-value delete) :error)
  (setf (avlnode2-balance delete) 0)
  nil)


;;
;;  operator
;;
(defun empty-avltree2 (inst)
  (declare (type avltree2 inst))
  (if (avltree2-root inst) nil t))

;;  min
(defun min-node-avlnode2 (node)
  (let ((left (avlnode2-left node)))
    (if left
      (min-node-avlnode2 left)
      node)))

(defun min-avlnode2 (x)
  (etypecase x
    (avlnode2
      (min-node-avlnode2 x))
    (avltree2
      (setq x (avltree2-root x))
      (when x
        (min-node-avlnode2 x)))))

(defun min-avltree2 (inst)
  (declare (type avltree2 inst))
  (let ((node (min-avlnode2 inst)))
    (if node
      (values (avlnode2-key node) t)
      (values nil nil))))

;;  max
(defun max-node-avlnode2 (node)
  (let ((right (avlnode2-right node)))
    (if right
      (max-node-avlnode2 right)
      node)))

(defun max-avlnode2 (x)
  (etypecase x
    (avlnode2
      (max-node-avlnode2 x))
    (avltree2
      (setq x (avltree2-root x))
      (when x
        (max-node-avlnode2 x)))))

(defun max-avltree2 (inst)
  (declare (type avltree2 inst))
  (let ((node (max-avlnode2 inst)))
    (if node
      (values (avlnode2-key node) t)
      (values nil nil))))

;;  map
(defun map-avlnode2 (call inst)
  (declare (type avltree2 inst))
  (labels ((rec (x) (when x
                      (rec (avlnode2-right x))
                      (funcall call x)
                      (rec (avlnode2-left x)))))
    (rec (avltree2-root inst)))
  (values))

(defun map-avltree2 (call inst)
  (declare (type avltree2 inst))
  (map-avlnode2
    (lambda (x)
      (funcall call (avlnode2-key x) (avlnode2-value x)))
    inst))

(defun keys-avltree2 (inst)
  (declare (type avltree2 inst))
  (let (list)
    (map-avlnode2
      (lambda (x)
        (push (avlnode2-key x) list))
      inst)
    list))

(defun values-avltree2 (inst)
  (declare (type avltree2 inst))
  (let (list)
    (map-avlnode2
      (lambda (x)
        (push (avlnode2-value x) list))
      inst)
    list))

(defun hash-table-avltree2 (inst &optional (test 'eql))
  (declare (type avltree2 inst))
  (let ((table (make-hash-table :test test)))
    (map-avltree2
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))

;;  parent
(defun next-avlnode2 (x)
  (prog (y z)
    (setq y (avlnode2-right x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (avlnode2-left y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (avlnode2-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (avlnode2-right y))
    (unless (eq x z)
      (return y))
    (setq x y y (avlnode2-parent y))
    (go parent-loop)))

(defun prev-avlnode2 (x)
  (prog (y z)
    (setq y (avlnode2-left x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (avlnode2-right y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (avlnode2-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (avlnode2-left y))
    (unless (eq x z)
      (return y))
    (setq x y y (avlnode2-parent y))
    (go parent-loop)))


;;
;;  rotate
;;
(defun rotate-left-avlnode2 (node)
  (let* ((p (avlnode2-parent node))
         (leftp (and p (eq (avlnode2-left p) node)))
         (x (avlnode2-right node))
         (y (avlnode2-left x)))
    (setf (avlnode2-right node) y)
    (when y
      (setf (avlnode2-parent y) node))
    (setf (avlnode2-left x) node)
    (setf (avlnode2-parent node) x)
    (if (null p)
      (setf (avlnode2-parent x) nil)
      (if leftp
        (setf (avlnode2-left p) x)
        (setf (avlnode2-right p) x)))
    x))

(defun rotate-right-avlnode2 (node)
  (let* ((p (avlnode2-parent node))
         (leftp (and p (eq (avlnode2-left p) node)))
         (x (avlnode2-left node))
         (y (avlnode2-right x)))
    (setf (avlnode2-left node) y)
    (when y
      (setf (avlnode2-parent y) node))
    (setf (avlnode2-right x) node)
    (setf (avlnode2-parent node) x)
    (if (null p)
      (setf (avlnode2-parent x) nil)
      (if leftp
        (setf (avlnode2-left p) x)
        (setf (avlnode2-right p) x)))
    x))


;;
;;  insert
;;
(defun replace-avlnode2 (node key value)
  (when *replace-key-avltree2*
    (setf (avlnode2-key node) key))
  (setf (avlnode2-value node) value))

(defun update-balance-avlnode2 (node)
  (let ((left (avlnode2-left node))
        (right (avlnode2-right node))
        (balance (avlnode2-balance node)))
    (cond ((= balance 1)
           (setf (avlnode2-balance left) 0)
           (setf (avlnode2-balance right) -1))
          ((= balance -1)
           (setf (avlnode2-balance left) 1)
           (setf (avlnode2-balance right) 0))
          (t (setf (avlnode2-balance left) 0)
             (setf (avlnode2-balance right) 0)))
    (setf (avlnode2-balance node) 0)
    node))

(defun insert-lr-avlnode2 (node left)
  (let ((left (rotate-left-avlnode2 left)))
    (setf (avlnode2-left node) left)
    (setf (avlnode2-parent left) node))
  (update-balance-avlnode2
    (rotate-right-avlnode2 node)))

(defun insert-ll-avlnode2 (node)
  (let ((next (rotate-right-avlnode2 node)))
    (setf (avlnode2-balance next) 0)
    (setf (avlnode2-balance node) 0)
    next))

(defun insert-left-avlnode2 (node)
  (let* ((left (avlnode2-left node))
         (balance (avlnode2-balance left)))
    (if (< balance 0)
      (insert-lr-avlnode2 node left)
      (insert-ll-avlnode2 node))))

(defun insert-rl-avlnode2 (node right)
  (let ((right (rotate-right-avlnode2 right)))
    (setf (avlnode2-right node) right)
    (setf (avlnode2-parent right) node))
  (update-balance-avlnode2
    (rotate-left-avlnode2 node)))

(defun insert-rr-avlnode2 (node)
  (let ((next (rotate-left-avlnode2 node)))
    (setf (avlnode2-balance next) 0)
    (setf (avlnode2-balance node) 0)
    next))

(defun insert-right-avlnode2 (node)
  (let* ((right (avlnode2-right node))
         (balance (avlnode2-balance right)))
    (if (< 0 balance)
      (insert-rl-avlnode2 node right)
      (insert-rr-avlnode2 node))))

(defun insert-avlnode2 (compare node key value)
  (declare (type (or avlnode2 null) node))
  (prog (key1 diff next check)
    ;;  nil
    (unless node
      (setq next (make-avlnode2 :key key :value value))
      (return (values next 'make)))

    ;;  move
    (setq key1 (avlnode2-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))

    ;;  equal
    (when *replace-mode-avltree2*
      (replace-avlnode2 node key value))
    (return (values nil nil))

    ;;  left
    move-left
    (setq next (avlnode2-left node))
    (multiple-value-setq (next check) (insert-avlnode2 compare next key value))
    (unless check
      (return (values nil nil)))
    (when (eq check t)
      (return (values node t)))
    (when (eq check 'finish)
      (setf (avlnode2-left node) next)
      (setf (avlnode2-parent next) node)
      (return (values node t)))
    (when (eq check 'make)
      (setf (avlnode2-left node) next)
      (setf (avlnode2-parent next) node))
    (incf (avlnode2-balance node) 1)
    (go balance)

    ;;  right
    move-right
    (setq next (avlnode2-right node))
    (multiple-value-setq (next check) (insert-avlnode2 compare next key value))
    (unless check
      (return (values nil nil)))
    (when (eq check t)
      (return (values node t)))
    (when (eq check 'finish)
      (setf (avlnode2-right node) next)
      (setf (avlnode2-parent next) node)
      (return (values node t)))
    (when (eq check 'make)
      (setf (avlnode2-right node) next)
      (setf (avlnode2-parent next) node))
    (decf (avlnode2-balance node) 1)
    (go balance)

    ;;  balance
    balance
    (setq check (avlnode2-balance node))
    (when (zerop check)
      (return (values node t)))  ;;  skip
    (when (< 1 check)  ;;  2
      (setq next (insert-left-avlnode2 node))
      (return (values next 'finish)))
    (when (< check -1)  ;;  -2
      (setq next (insert-right-avlnode2 node))
      (return (values next 'finish)))
    ;;  1 or -1
    (return (values node 'loop))))

(defun insert-avltree2
  (inst key value &optional (*replace-mode-avltree2* *replace-mode-avltree2*))
  (declare (type avltree2 inst))
  (let ((compare (avltree2-compare inst))
        (root (avltree2-root inst)))
    (multiple-value-bind (node check) (insert-avlnode2 compare root key value)
      (when check
        (setf (avltree2-root inst) node)
        (incf (avltree2-size inst) 1)
        t))))

(defun intern-avltree2 (inst key value)
  (declare (type avltree2 inst))
  (insert-avltree2 inst key value t))


;;
;;  search
;;
(defun search-avlnode2 (inst key)
  (declare (type avltree2 inst))
  (prog ((node (avltree2-root inst))
         (compare (avltree2-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (avlnode2-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (avlnode2-left node)))
          ((< 0 diff) (setq node (avlnode2-right node)))
          (t (return node)))
    (go loop)))

(defun search-avltree2 (inst key)
  (declare (type avltree2 inst))
  (let ((node (search-avlnode2 inst key)))
    (if node
      (values (avlnode2-value node) t)
      (values nil nil))))

(defun replace-avltree2
  (inst key value &optional (*replace-key-avltree2* *replace-key-avltree2*))
  (declare (type avltree2 inst))
  (let ((node (search-avlnode2 inst key)))
    (when node
      (replace-avlnode2 node key value)
      t)))


;;
;;  delete
;;
(defun delete-copy-avlnode2 (replace delete)
  (setf (avlnode2-key replace) (avlnode2-key delete))
  (setf (avlnode2-value replace) (avlnode2-value delete))
  (free-avlnode2 delete))

(defun delete-single-avlnode2 (node delete diff)
  (let* ((left (avlnode2-left delete))
         (right (avlnode2-right delete))
         (next (or left right)))
    (if (< diff 0)
      (setf (avlnode2-left node) next)
      (setf (avlnode2-right node) next))
    (when next
      (setf (avlnode2-parent next) node))
    (free-avlnode2 delete)))

(defun delete-lr-avlnode2 (node left)
  (let ((left (rotate-left-avlnode2 left)))
    (setf (avlnode2-left node) left)
    (setf (avlnode2-parent left) node))
  (update-balance-avlnode2
    (rotate-right-avlnode2 node)))

(defun delete-ll-avlnode2 (node)
  (let ((next (rotate-right-avlnode2 node)))
    (if (zerop (avlnode2-balance next))
      (setf (avlnode2-balance next) -1
            (avlnode2-balance node) 1)
      (setf (avlnode2-balance next) 0
            (avlnode2-balance node) 0))
    next))

(defun delete-left-avlnode2 (node)
  (let* ((left (avlnode2-left node))
         (balance (avlnode2-balance left)))
    (if (< balance 0)
      (delete-lr-avlnode2 node left)
      (delete-ll-avlnode2 node))))

(defun delete-rl-avlnode2 (node right)
  (let ((right (rotate-right-avlnode2 right)))
    (setf (avlnode2-right node) right)
    (setf (avlnode2-parent right) node))
  (update-balance-avlnode2
    (rotate-left-avlnode2 node)))

(defun delete-rr-avlnode2 (node)
  (let ((next (rotate-left-avlnode2 node)))
    (if (zerop (avlnode2-balance next))
      (setf (avlnode2-balance next) 1
            (avlnode2-balance node) -1)
      (setf (avlnode2-balance next) 0
            (avlnode2-balance node) 0))
    next))

(defun delete-right-avlnode2 (node)
  (let* ((right (avlnode2-right node))
         (balance (avlnode2-balance right)))
    (if (< 0 balance)
      (delete-rl-avlnode2 node right)
      (delete-rr-avlnode2 node))))

(defun delete-balance-avlnode2 (node)
  (prog (next check)
    (setq check (avlnode2-balance node))
    (when (zerop check)  ;;  0
      (return (values nil 'loop)))
    (when (or (= check 1) (= check -1))  ;;  -1, 1
      (return (values nil t)))
    (when (< 1 check)  ;;  2
      (setq next (delete-left-avlnode2 node)))
    (when (< check -1)  ;;  -2
      (setq next (delete-right-avlnode2 node)))
    ;;  -2, 2
    (return (values next 'update))))

(defun delete-swap-avlnode2 (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  loop
    (setq next (avlnode2-left node))
    (multiple-value-setq (next check) (delete-swap-avlnode2 replace next))
    (unless check
      (return (values node 'replace)))
    (when (eq check t)
      (return (values nil t)))
    (when (eq check 'replace)
      (let ((right (avlnode2-right next)))
        (setf (avlnode2-left node) right)
        (when right
          (setf (avlnode2-parent right) node)))
      (delete-copy-avlnode2 replace next))
    (when (eq check 'update)
      (setf (avlnode2-left node) next)
      (setf (avlnode2-parent next) node)
      (unless (zerop (avlnode2-balance next))
        (return (values nil t))))

    ;;  loop, update
    (decf (avlnode2-balance node) 1)  ;; left
    (return (delete-balance-avlnode2 node))))

(defun delete-replace-avlnode2 (node next)
  (let ((right (avlnode2-right next)))
    (setf (avlnode2-right node) right)
    (when right
      (setf (avlnode2-parent right) node)))
  (delete-copy-avlnode2 node next)
  (incf (avlnode2-balance node) 1)  ;; right
  (delete-balance-avlnode2 node))

(defun delete-avlnode2 (compare node key)
  (prog (key1 diff left next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (avlnode2-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (setq next (avlnode2-left node))
      (go loop-move))
    (when (< 0 diff)
      (setq next (avlnode2-right node))
      (go loop-move))

    ;;  delete
    (setq left (avlnode2-left node))
    (setq next (avlnode2-right node))
    (unless (and left next)
      (return (values node 'delete)))
    (multiple-value-setq (next check) (delete-swap-avlnode2 node next))
    (when (eq check 'replace)
      (return (delete-replace-avlnode2 node next)))
    (go loop-result)

    ;;  loop
    loop-move
    (multiple-value-setq (next check) (delete-avlnode2 compare next key))
    (unless check
      (return (values nil nil)))

    loop-result
    (when (eq check t)
      (return (values nil t)))
    (when (eq check 'delete)
      (delete-single-avlnode2 node next diff))
    (when (eq check 'update)
      (if (< diff 0)
        (setf (avlnode2-left node) next
              (avlnode2-parent next) node)
        (setf (avlnode2-right node) next
              (avlnode2-parent next) node))
      (unless (zerop (avlnode2-balance next))
        (return (values nil t))))

    ;;  loop, delete, update
    (if (< diff 0)
      (decf (avlnode2-balance node) 1)
      (incf (avlnode2-balance node) 1))
    (return (delete-balance-avlnode2 node))))

(defun delete-setf-avltree2 (inst node)
  (setf (avltree2-root inst) node)
  (setf (avlnode2-parent node) nil))

(defun delete-delete-avltree2 (inst delete)
  (let* ((left (avlnode2-left delete))
         (right (avlnode2-right delete))
         (next (or left right)))
    (setf (avltree2-root inst) next)
    (when next
      (setf (avlnode2-parent next) nil))
    (free-avlnode2 delete)))

(defun delete-avltree2 (inst key)
  (declare (type avltree2 inst))
  (let ((compare (avltree2-compare inst))
        (root (avltree2-root inst)))
    (multiple-value-bind (node check) (delete-avlnode2 compare root key)
      (when check
        (ecase check
          ((t loop) nil)
          (delete (delete-delete-avltree2 inst node))
          (update (delete-setf-avltree2 inst node)))
        (decf (avltree2-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-avltree2-struct
  call depth limit lower direct)

(defun init-loop-avltree2 (str now)
  (prog* ((call (init-avltree2-struct-call str))
          (depth (init-avltree2-struct-depth str))
          (limit (init-avltree2-struct-limit str))
          (lower (init-avltree2-struct-lower str))
          (direct (init-avltree2-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-avltree2-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-avltree2 str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-avltree2 str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-avlnode2
                 :key key :value value :balance (- x y)
                 :left left :right right))
    (when left
      (setf (avlnode2-parent left) node))
    (when right
      (setf (avlnode2-parent right) node))
    (return (values node (max x y)))))

(defun init-call-avltree2 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-avltree2
      (make-init-avltree2-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-avltree2 (call size &optional inst direct)
  (unless inst
    (setq inst (make-avltree2)))
  (setf (avltree2-root inst) (init-call-avltree2 call size direct))
  (setf (avltree2-size inst) size)
  inst)

(defun init-property1-avltree2 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-avltree2 (seq size)
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

(defun init-property-avltree2 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-avltree2
        (if (listp seq)
          (init-property1-avltree2 seq)
          (init-property2-avltree2 seq div))
        div inst))))

(defun init-associate1-avltree2 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-avltree2 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-avltree2 (inst seq)
  (let ((size (length seq)))
    (init-avltree2
      (if (listp seq)
        (init-associate1-avltree2 seq)
        (init-associate2-avltree2 seq size))
      size inst)))

