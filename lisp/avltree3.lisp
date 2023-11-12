;;
;;  AVL Tree  (fixup)
;;
(defpackage #:avltree3
  (:use common-lisp)
  (:export
    #:avltree3
    #:avltree3-p
    #:make-avltree3
    #:clear-avltree3
    #:size-avltree3
    #:avlnode3-p
    #:key-avlnode3
    #:value-avlnode3

    #:empty-avltree3
    #:min-avltree3
    #:min-avlnode3
    #:max-avltree3
    #:max-avlnode3
    #:map-avltree3
    #:map-avlnode3
    #:keys-avltree3
    #:values-avltree3
    #:hash-table-avltree3
    #:next-avlnode3
    #:prev-avlnode3

    #:*replace-mode-avltree3*
    #:*replace-key-avltree3*
    #:insert-avltree3
    #:intern-avltree3
    #:search-avltree3
    #:search-avlnode3
    #:replace-avltree3
    #:delete-avltree3
    #:init-avltree3
    #:init-property-avltree3
    #:init-associate-avltree3))

(in-package #:avltree3)

(defvar *replace-mode-avltree3* nil)
(defvar *replace-key-avltree3* t)


;;
;;  avltree3
;;
(defstruct (avltree3
             (:constructor avltree3-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst avltree3) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avltree3-size inst))))

(defun make-avltree3 (&key (compare #'-))
  (avltree3-heap :compare compare))

(defun clear-avltree3 (inst)
  (declare (type avltree3 inst))
  (setf (avltree3-root inst) nil)
  (setf (avltree3-size inst) 0))

(defun size-avltree3 (inst)
  (declare (type avltree3 inst))
  (avltree3-size inst))


;;
;;  avlnode3
;;
(defstruct (avlnode3 (:copier nil))
  parent left right key value
  (balance 0 :type (integer -2 2)))  ;; left - right

(defmethod print-object ((inst avlnode3) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (avlnode3-balance inst))))

(defun key-avlnode3 (inst)
  (declare (type avlnode3 inst))
  (avlnode3-key inst))

(defun value-avlnode3 (inst)
  (declare (type avlnode3 inst))
  (avlnode3-value inst))

(defun free-avlnode3 (delete)
  (setf (avlnode3-parent delete) :error)
  (setf (avlnode3-left delete) :error)
  (setf (avlnode3-right delete) :error)
  (setf (avlnode3-key delete) :error)
  (setf (avlnode3-value delete) :error)
  (setf (avlnode3-balance delete) 0)
  nil)


;;
;;  operator
;;
(defun empty-avltree3 (inst)
  (declare (type avltree3 inst))
  (if (avltree3-root inst) nil t))

;;  min
(defun min-node-avlnode3 (node)
  (let ((left (avlnode3-left node)))
    (if left
      (min-node-avlnode3 left)
      node)))

(defun min-avlnode3 (x)
  (etypecase x
    (avlnode3
      (min-node-avlnode3 x))
    (avltree3
      (setq x (avltree3-root x))
      (when x
        (min-node-avlnode3 x)))))

(defun min-avltree3 (inst)
  (declare (type avltree3 inst))
  (let ((node (min-avlnode3 inst)))
    (if node
      (values (avlnode3-key node) t)
      (values nil nil))))

;;  max
(defun max-node-avlnode3 (node)
  (let ((right (avlnode3-right node)))
    (if right
      (max-node-avlnode3 right)
      node)))

(defun max-avlnode3 (x)
  (etypecase x
    (avlnode3
      (max-node-avlnode3 x))
    (avltree3
      (setq x (avltree3-root x))
      (when x
        (max-node-avlnode3 x)))))

(defun max-avltree3 (inst)
  (declare (type avltree3 inst))
  (let ((node (max-avlnode3 inst)))
    (if node
      (values (avlnode3-key node) t)
      (values nil nil))))

;;  map
(defun map-avlnode3 (call inst)
  (declare (type avltree3 inst))
  (labels ((rec (x) (when x
                      (rec (avlnode3-right x))
                      (funcall call x)
                      (rec (avlnode3-left x)))))
    (rec (avltree3-root inst)))
  (values))

(defun map-avltree3 (call inst)
  (declare (type avltree3 inst))
  (map-avlnode3
    (lambda (x)
      (funcall call (avlnode3-key x) (avlnode3-value x)))
    inst))

(defun keys-avltree3 (inst)
  (declare (type avltree3 inst))
  (let (list)
    (map-avlnode3
      (lambda (x)
        (push (avlnode3-key x) list))
      inst)
    list))

(defun values-avltree3 (inst)
  (declare (type avltree3 inst))
  (let (list)
    (map-avlnode3
      (lambda (x)
        (push (avlnode3-value x) list))
      inst)
    list))

(defun hash-table-avltree3 (inst &optional (test 'eql))
  (declare (type avltree3 inst))
  (let ((table (make-hash-table :test test)))
    (map-avltree3
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))

;;  parent
(defun next-avlnode3 (x)
  (prog (y z)
    (setq y (avlnode3-right x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (avlnode3-left y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (avlnode3-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (avlnode3-right y))
    (unless (eq x z)
      (return y))
    (setq x y y (avlnode3-parent y))
    (go parent-loop)))

(defun prev-avlnode3 (x)
  (prog (y z)
    (setq y (avlnode3-left x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (avlnode3-right y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (avlnode3-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (avlnode3-left y))
    (unless (eq x z)
      (return y))
    (setq x y y (avlnode3-parent y))
    (go parent-loop)))


;;
;;  rotate
;;
(defun rotate-left-avlnode3 (node)
  (let* ((p (avlnode3-parent node))
         (leftp (and p (eq (avlnode3-left p) node)))
         (x (avlnode3-right node))
         (y (avlnode3-left x)))
    (setf (avlnode3-right node) y)
    (when y
      (setf (avlnode3-parent y) node))
    (setf (avlnode3-left x) node)
    (setf (avlnode3-parent node) x)
    (if (null p)
      (setf (avlnode3-parent x) nil)
      (if leftp
        (setf (avlnode3-left p) x)
        (setf (avlnode3-right p) x)))
    x))

(defun rotate-right-avlnode3 (node)
  (let* ((p (avlnode3-parent node))
         (leftp (and p (eq (avlnode3-left p) node)))
         (x (avlnode3-left node))
         (y (avlnode3-right x)))
    (setf (avlnode3-left node) y)
    (when y
      (setf (avlnode3-parent y) node))
    (setf (avlnode3-right x) node)
    (setf (avlnode3-parent node) x)
    (if (null p)
      (setf (avlnode3-parent x) nil)
      (if leftp
        (setf (avlnode3-left p) x)
        (setf (avlnode3-right p) x)))
    x))


;;
;;  insert fixup
;;
(defun update-balance-avlnode3 (node)
  (let ((left (avlnode3-left node))
        (right (avlnode3-right node))
        (balance (avlnode3-balance node)))
    (cond ((= balance 1)
           (setf (avlnode3-balance left) 0)
           (setf (avlnode3-balance right) -1))
          ((= balance -1)
           (setf (avlnode3-balance left) 1)
           (setf (avlnode3-balance right) 0))
          (t (setf (avlnode3-balance left) 0)
             (setf (avlnode3-balance right) 0)))
    (setf (avlnode3-balance node) 0)
    node))

(defun insert-lr-avlnode3 (node left)
  (let ((left (rotate-left-avlnode3 left)))
    (setf (avlnode3-left node) left)
    (setf (avlnode3-parent left) node))
  (update-balance-avlnode3
    (rotate-right-avlnode3 node)))

(defun insert-ll-avlnode3 (node)
  (let ((next (rotate-right-avlnode3 node)))
    (setf (avlnode3-balance next) 0)
    (setf (avlnode3-balance node) 0)
    next))

(defun insert-left-avlnode3 (node)
  (let* ((left (avlnode3-left node))
         (balance (avlnode3-balance left)))
    (if (< balance 0)
      (insert-lr-avlnode3 node left)
      (insert-ll-avlnode3 node))))

(defun insert-rl-avlnode3 (node right)
  (let ((right (rotate-right-avlnode3 right)))
    (setf (avlnode3-right node) right)
    (setf (avlnode3-parent right) node))
  (update-balance-avlnode3
    (rotate-left-avlnode3 node)))

(defun insert-rr-avlnode3 (node)
  (let ((next (rotate-left-avlnode3 node)))
    (setf (avlnode3-balance next) 0)
    (setf (avlnode3-balance node) 0)
    next))

(defun insert-right-avlnode3 (node)
  (let* ((right (avlnode3-right node))
         (balance (avlnode3-balance right)))
    (if (< 0 balance)
      (insert-rl-avlnode3 node right)
      (insert-rr-avlnode3 node))))

(defun insert-fixup-avltree3 (inst node)
  (prog (node1 node2 left check)
    ;;  insert1
    (setq node1 (avlnode3-parent node))
    (unless node1
      (return nil))

    ;;  insert2
    fixup-loop
    (setq node2 (avlnode3-parent node1))
    (when node2
      (setq left (avlnode3-left node2)))
    (if (eq (avlnode3-left node1) node)
      (incf (avlnode3-balance node1) 1)
      (decf (avlnode3-balance node1) 1))
    (setq check (avlnode3-balance node1))
    (when (zerop check)  ;; skip
      (return nil))
    (when (< 1 check)  ;; 2
      (setq node (insert-left-avlnode3 node1))
      (go fixup-next))
    (when (< check -1)  ;; -2
      (setq node (insert-right-avlnode3 node1))
      (go fixup-next))
    ;;  -1 or 1
    (setq node node1)
    (setq node1 (avlnode3-parent node))
    (when node1
      (go fixup-loop))
    (return nil)

    fixup-next
    (unless node2
      (setf (avltree3-root inst) node)
      (return nil))
    (if (eq left node1)
      (setf (avlnode3-left node2) node)
      (setf (avlnode3-right node2) node))
    (setf (avlnode3-parent node) node2)))


;;
;;  insert
;;
(defun replace-avlnode3 (node key value)
  (when *replace-key-avltree3*
    (setf (avlnode3-key node) key))
  (setf (avlnode3-value node) value))

(defun insert-avlnode3 (compare node key value)
  (prog (key1 diff next)
    loop
    (setq key1 (avlnode3-key node))
    (setq diff (funcall compare key key1))

    ;; left
    (when (< diff 0)
      (setq next (avlnode3-left node))
      (unless next
        (setq next (make-avlnode3 :key key :value value))
        (setf (avlnode3-left node) next)
        (setf (avlnode3-parent next) node)
        (return next))
      (setq node next)
      (go loop))

    ;; right
    (when (< 0 diff)
      (setq next (avlnode3-right node))
      (unless next
        (setq next (make-avlnode3 :key key :value value))
        (setf (avlnode3-right node) next)
        (setf (avlnode3-parent next) node)
        (return next))
      (setq node next)
      (go loop))

    ;;  equal
    (when *replace-mode-avltree3*
      (replace-avlnode3 node key value))
    (return nil)))

(defun insert2-avltree3 (inst root key value)
  (let* ((compare (avltree3-compare inst))
         (node (insert-avlnode3 compare root key value)))
    (when node
      (incf (avltree3-size inst) 1)
      node)))

(defun insert1-avltree3 (inst key value)
  (let ((node (make-avlnode3 :key key :value value)))
    (setf (avltree3-root inst) node)
    (setf (avltree3-size inst) 1)
    node))

(defun insert0-avltree3 (inst key value)
  (let ((root (avltree3-root inst)))
    (if root
      (insert2-avltree3 inst root key value)
      (insert1-avltree3 inst key value))))

(defun insert-avltree3
  (inst key value &optional (*replace-mode-avltree3* *replace-mode-avltree3*))
  (let ((node (insert0-avltree3 inst key value)))
    (when node
      (insert-fixup-avltree3 inst node)
      t)))

(defun intern-avltree3 (inst key value)
  (declare (type avltree3 inst))
  (insert-avltree3 inst key value t))


;;
;;  search
;;
(defun search-avlnode3 (inst key)
  (declare (type avltree3 inst))
  (prog ((node (avltree3-root inst))
         (compare (avltree3-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (avlnode3-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (avlnode3-left node)))
          ((< 0 diff) (setq node (avlnode3-right node)))
          (t (return node)))
    (go loop)))

(defun search-avltree3 (inst key)
  (declare (type avltree3 inst))
  (let ((node (search-avlnode3 inst key)))
    (if node
      (values (avlnode3-value node) t)
      (values nil nil))))

(defun replace-avltree3
  (inst key value &optional (*replace-key-avltree3* *replace-key-avltree3*))
  (declare (type avltree3 inst))
  (let ((node (search-avlnode3 inst key)))
    (when node
      (replace-avlnode3 node key value)
      t)))


;;
;;  delete fixup
;;
(defun delete-lr-avlnode3 (node left)
  (let ((left (rotate-left-avlnode3 left)))
    (setf (avlnode3-left node) left)
    (setf (avlnode3-parent left) node))
  (update-balance-avlnode3
    (rotate-right-avlnode3 node)))

(defun delete-ll-avlnode3 (node)
  (let ((next (rotate-right-avlnode3 node)))
    (if (zerop (avlnode3-balance next))
      (setf (avlnode3-balance next) -1
            (avlnode3-balance node) 1)
      (setf (avlnode3-balance next) 0
            (avlnode3-balance node) 0))
    next))

(defun delete-left-avlnode3 (node)
  (let* ((left (avlnode3-left node))
         (balance (avlnode3-balance left)))
    (if (< balance 0)
      (delete-lr-avlnode3 node left)
      (delete-ll-avlnode3 node))))

(defun delete-rl-avlnode3 (node right)
  (let ((right (rotate-right-avlnode3 right)))
    (setf (avlnode3-right node) right)
    (setf (avlnode3-parent right) node))
  (update-balance-avlnode3
    (rotate-left-avlnode3 node)))

(defun delete-rr-avlnode3 (node)
  (let ((next (rotate-left-avlnode3 node)))
    (if (zerop (avlnode3-balance next))
      (setf (avlnode3-balance next) 1
            (avlnode3-balance node) -1)
      (setf (avlnode3-balance next) 0
            (avlnode3-balance node) 0))
    next))

(defun delete-right-avlnode3 (node)
  (let* ((right (avlnode3-right node))
         (balance (avlnode3-balance right)))
    (if (< 0 balance)
      (delete-rl-avlnode3 node right)
      (delete-rr-avlnode3 node))))

(defun delete-fixup-avltree3 (inst node direct)
  (prog (leftp left node1 next check)
    (unless direct
      (return nil))
    (setq leftp (eq direct 'left))

    fixup-loop
    (setq node1 (avlnode3-parent node))
    (when node1
      (setq left (avlnode3-left node1)))
    (if leftp
      (decf (avlnode3-balance node) 1)
      (incf (avlnode3-balance node) 1))
    (setq check (avlnode3-balance node))
    (when (or (= check 1) (= check -1))   ;;  -1, 1
      (return nil))
    (when (< 1 check)  ;;  2
      (setq next (delete-left-avlnode3 node))
      (go fixup-next))
    (when (< check -1)  ;;  -2
      (setq next (delete-right-avlnode3 node))
      (go fixup-next))
    ;;  0
    (unless node1
      (return nil))
    (setq leftp (eq (avlnode3-left node1) node))
    (setq node node1)
    (go fixup-loop)

    fixup-next
    (unless node1
      (setf (avltree3-root inst) next)
      (return nil))
    (if (eq left node)
      (setf (avlnode3-left node1) next)
      (setf (avlnode3-right node1) next))
    (setf (avlnode3-parent next) node1)
    (unless (zerop (avlnode3-balance next))
      (return nil))
    (setq leftp (eq (avlnode3-left node1) next))
    (setq node node1)
    (go fixup-loop)))


;;
;;  delete
;;
(defun delete-copy-avlnode3 (replace delete)
  (setf (avlnode3-key replace) (avlnode3-key delete))
  (setf (avlnode3-value replace) (avlnode3-value delete)))

(defun delete-single-avlnode3 (inst delete child)
  (let ((parent (avlnode3-parent delete)) direct)
    (if parent
      (if (eq (avlnode3-left parent) delete)
        (setf (avlnode3-left parent) child direct 'left)
        (setf (avlnode3-right parent) child direct 'right))
      (setf (avltree3-root inst) child direct nil))
    (when child
      (setf (avlnode3-parent child) parent))
    (delete-fixup-avltree3 inst parent direct)
    (free-avlnode3 delete)))

(defun delete-swap-avlnode3 (inst replace)
  (let* ((delete (next-avlnode3 replace))
         (right (avlnode3-right delete)))
    (delete-copy-avlnode3 replace delete)
    (delete-single-avlnode3 inst delete right)))

(defun delete-avlnode3 (inst delete)
  (let ((left (avlnode3-left delete))
        (right (avlnode3-right delete)))
    (if (and left right)
      (delete-swap-avlnode3 inst delete)
      (delete-single-avlnode3 inst delete (or left right)))))

(defun delete-avltree3 (inst key)
  (declare (type avltree3 inst))
  (let ((delete (search-avlnode3 inst key)))
    (when delete
      (delete-avlnode3 inst delete)
      (decf (avltree3-size inst) 1)
      t)))


;;
;;  init
;;
(defstruct init-avltree3-struct
  call depth limit lower direct)

(defun init-loop-avltree3 (str now)
  (prog* ((call (init-avltree3-struct-call str))
          (depth (init-avltree3-struct-depth str))
          (limit (init-avltree3-struct-limit str))
          (lower (init-avltree3-struct-lower str))
          (direct (init-avltree3-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-avltree3-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-avltree3 str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-avltree3 str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-avlnode3
                 :key key :value value :balance (- x y)
                 :left left :right right))
    (when left
      (setf (avlnode3-parent left) node))
    (when right
      (setf (avlnode3-parent right) node))
    (return (values node (max x y)))))

(defun init-call-avltree3 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-avltree3
      (make-init-avltree3-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-avltree3 (call size &optional inst direct)
  (unless inst
    (setq inst (make-avltree3)))
  (setf (avltree3-root inst) (init-call-avltree3 call size direct))
  (setf (avltree3-size inst) size)
  inst)

(defun init-property1-avltree3 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-avltree3 (seq size)
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

(defun init-property-avltree3 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-avltree3
        (if (listp seq)
          (init-property1-avltree3 seq)
          (init-property2-avltree3 seq div))
        div inst))))

(defun init-associate1-avltree3 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-avltree3 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-avltree3 (inst seq)
  (let ((size (length seq)))
    (init-avltree3
      (if (listp seq)
        (init-associate1-avltree3 seq)
        (init-associate2-avltree3 seq size))
      size inst)))

