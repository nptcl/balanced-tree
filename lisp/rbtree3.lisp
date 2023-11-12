;;
;;  Red-Black Tree  (fixup)
;;
(defpackage #:rbtree3
  (:use common-lisp)
  (:export
    #:rbtree3
    #:rbtree3-p
    #:make-rbtree3
    #:clear-rbtree3
    #:size-rbtree3
    #:rbnode3-p
    #:key-rbnode3
    #:value-rbnode3

    #:empty-rbtree3
    #:min-rbtree3
    #:min-rbnode3
    #:max-rbtree3
    #:max-rbnode3
    #:map-rbtree3
    #:map-rbnode3
    #:keys-rbtree3
    #:values-rbtree3
    #:hash-table-rbtree3
    #:next-rbnode3
    #:prev-rbnode3

    #:*replace-mode-rbtree3*
    #:*replace-key-rbtree3*
    #:insert-rbtree3
    #:intern-rbtree3
    #:search-rbtree3
    #:search-rbnode3
    #:replace-rbtree3
    #:delete-rbtree3
    #:init-rbtree3
    #:init-property-rbtree3
    #:init-associate-rbtree3))

(in-package #:rbtree3)

(defvar *replace-mode-rbtree3* nil)
(defvar *replace-key-rbtree3* t)


;;
;;  rbtree3
;;
(defstruct (rbtree3
             (:constructor rbtree3-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst rbtree3) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (rbtree3-size inst))))

(defun make-rbtree3 (&key (compare #'-))
  (rbtree3-heap :compare compare))

(defun clear-rbtree3 (inst)
  (declare (type rbtree3 inst))
  (setf (rbtree3-root inst) nil)
  (setf (rbtree3-size inst) 0))

(defun size-rbtree3 (inst)
  (declare (type rbtree3 inst))
  (rbtree3-size inst))


;;
;;  rbnode3
;;
(defstruct (rbnode3 (:copier nil))
  parent left right key value
  (color 'red :type (member red black :error)))

(defmethod print-object ((inst rbnode3) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (rbnode3-key inst))))

(defun key-rbnode3 (inst)
  (declare (type rbnode3 inst))
  (rbnode3-key inst))

(defun value-rbnode3 (inst)
  (declare (type rbnode3 inst))
  (rbnode3-value inst))

(defun rbnode3-error-check (node)
  (declare (type (or rbnode3 null) node))
  (when (and node (eq (rbnode3-color node) :error))
    (error "rbnode3-color error, ~S." node))
  (values))

(defun rbnode3-red-p (node)
  (declare (type rbnode3 node))
  (rbnode3-error-check node)
  (eq (rbnode3-color node) 'red))

(defun rbnode3-black-p (node)
  (declare (type rbnode3 node))
  (rbnode3-error-check node)
  (eq (rbnode3-color node) 'black))

(defun rbnode3-red-p! (node)
  (rbnode3-error-check node)
  (and node
       (eq (rbnode3-color node) 'red)))

(defun rbnode3-black-p! (node)
  (rbnode3-error-check node)
  (or (null node)
      (eq (rbnode3-color node) 'black)))

(defun setf-rbnode3-red (node)
  (declare (type rbnode3 node))
  (rbnode3-error-check node)
  (setf (rbnode3-color node) 'red))

(defun setf-rbnode3-black (node)
  (declare (type rbnode3 node))
  (rbnode3-error-check node)
  (setf (rbnode3-color node) 'black))

(defun free-rbnode3 (delete)
  (setf (rbnode3-parent delete) :error)
  (setf (rbnode3-left delete) :error)
  (setf (rbnode3-right delete) :error)
  (setf (rbnode3-key delete) :error)
  (setf (rbnode3-value delete) :error)
  (setf (rbnode3-color delete) :error)
  nil)


;;
;;  operator
;;
(defun empty-rbtree3 (inst)
  (declare (type rbtree3 inst))
  (if (rbtree3-root inst) nil t))

;;  min
(defun min-node-rbnode3 (node)
  (let ((left (rbnode3-left node)))
    (if left
      (min-node-rbnode3 left)
      node)))

(defun min-rbnode3 (x)
  (etypecase x
    (rbnode3
      (min-node-rbnode3 x))
    (rbtree3
      (setq x (rbtree3-root x))
      (when x
        (min-node-rbnode3 x)))))

(defun min-rbtree3 (inst)
  (declare (type rbtree3 inst))
  (let ((node (min-rbnode3 inst)))
    (if node
      (values (rbnode3-key node) t)
      (values nil nil))))

;;  max
(defun max-node-rbnode3 (node)
  (let ((right (rbnode3-right node)))
    (if right
      (max-node-rbnode3 right)
      node)))

(defun max-rbnode3 (x)
  (etypecase x
    (rbnode3
      (max-node-rbnode3 x))
    (rbtree3
      (setq x (rbtree3-root x))
      (when x
        (max-node-rbnode3 x)))))

(defun max-rbtree3 (inst)
  (declare (type rbtree3 inst))
  (let ((node (max-rbnode3 inst)))
    (if node
      (values (rbnode3-key node) t)
      (values nil nil))))

;;  map
(defun map-rbnode3 (call inst)
  (declare (type rbtree3 inst))
  (labels ((rec (x) (when x
                      (rec (rbnode3-right x))
                      (funcall call x)
                      (rec (rbnode3-left x)))))
    (rec (rbtree3-root inst)))
  (values))

(defun map-rbtree3 (call inst)
  (declare (type rbtree3 inst))
  (map-rbnode3
    (lambda (x)
      (funcall call (rbnode3-key x) (rbnode3-value x)))
    inst))

(defun keys-rbtree3 (inst)
  (declare (type rbtree3 inst))
  (let (list)
    (map-rbnode3
      (lambda (x)
        (push (rbnode3-key x) list))
      inst)
    list))

(defun values-rbtree3 (inst)
  (declare (type rbtree3 inst))
  (let (list)
    (map-rbnode3
      (lambda (x)
        (push (rbnode3-value x) list))
      inst)
    list))

(defun hash-table-rbtree3 (inst &optional (test 'eql))
  (declare (type rbtree3 inst))
  (let ((table (make-hash-table :test test)))
    (map-rbtree3
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))

;;  parent
(defun next-rbnode3 (x)
  (prog (y z)
    (setq y (rbnode3-right x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (rbnode3-left y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (rbnode3-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (rbnode3-right y))
    (unless (eq x z)
      (return y))
    (setq x y y (rbnode3-parent y))
    (go parent-loop)))

(defun prev-rbnode3 (x)
  (prog (y z)
    (setq y (rbnode3-left x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (rbnode3-right y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (rbnode3-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (rbnode3-left y))
    (unless (eq x z)
      (return y))
    (setq x y y (rbnode3-parent y))
    (go parent-loop)))


;;
;;  rotate
;;
(defun rotate-left-rbnode3 (node)
  (let* ((p (rbnode3-parent node))
         (leftp (and p (eq (rbnode3-left p) node)))
         (x (rbnode3-right node))
         (y (rbnode3-left x)))
    (setf (rbnode3-right node) y)
    (when y
      (setf (rbnode3-parent y) node))
    (setf (rbnode3-left x) node)
    (setf (rbnode3-parent node) x)
    (if (null p)
      (setf (rbnode3-parent x) nil)
      (if leftp
        (setf (rbnode3-left p) x)
        (setf (rbnode3-right p) x)))
    (setf (rbnode3-color x) (rbnode3-color node))
    (setf-rbnode3-red node)
    x))

(defun rotate-right-rbnode3 (node)
  (let* ((p (rbnode3-parent node))
         (leftp (and p (eq (rbnode3-left p) node)))
         (x (rbnode3-left node))
         (y (rbnode3-right x)))
    (setf (rbnode3-left node) y)
    (when y
      (setf (rbnode3-parent y) node))
    (setf (rbnode3-right x) node)
    (setf (rbnode3-parent node) x)
    (if (null p)
      (setf (rbnode3-parent x) nil)
      (if leftp
        (setf (rbnode3-left p) x)
        (setf (rbnode3-right p) x)))
    (setf (rbnode3-color x) (rbnode3-color node))
    (setf-rbnode3-red node)
    x))


;;
;;  insert fixup
;;
(defun insert-left-rbnode3 (node)
  (prog (left right next)
    (when (rbnode3-red-p node)
      (return (values node t)))

    ;;  black
    (setq left (rbnode3-left node))
    (setq next (rbnode3-right left))
    (unless next
      (go next-check))
    (when (rbnode3-black-p next)
      (go next-check))
    (setq left (rotate-left-rbnode3 left))
    (setf (rbnode3-left node) left)
    (setf (rbnode3-parent left) node)

    next-check
    (setq next (rbnode3-left left))
    (unless next
      (go return-balanced))
    (when (rbnode3-black-p next)
      (go return-balanced))

    ;;  red-red
    (setq right (rbnode3-right node))
    (unless right
      (go rotate-right))
    (when (rbnode3-black-p right)
      (go rotate-right))
    (setf-rbnode3-red node)
    (setf-rbnode3-black left)
    (setf-rbnode3-black right)
    (return (values node t))

    rotate-right
    (setq node (rotate-right-rbnode3 node))
    return-balanced
    (return (values node nil))))

(defun insert-right-rbnode3 (node)
  (prog (left right next)
    (when (rbnode3-red-p node)
      (return (values node t)))

    ;;  black
    (setq right (rbnode3-right node))
    (setq next (rbnode3-left right))
    (unless next
      (go next-check))
    (when (rbnode3-black-p next)
      (go next-check))
    (setq right (rotate-right-rbnode3 right))
    (setf (rbnode3-right node) right)
    (setf (rbnode3-parent right) node)

    next-check
    (setq next (rbnode3-right right))
    (unless next
      (go return-balanced))
    (when (rbnode3-black-p next)
      (go return-balanced))

    ;;  red-red
    (setq left (rbnode3-left node))
    (unless left
      (go rotate-left))
    (when (rbnode3-black-p left)
      (go rotate-left))
    (setf-rbnode3-red node)
    (setf-rbnode3-black left)
    (setf-rbnode3-black right)
    (return (values node t))

    rotate-left
    (setq node (rotate-left-rbnode3 node))
    return-balanced
    (return (values node nil))))

(defun insert-fixup-rbtree3 (inst node)
  (prog (node1 node2 left check)
    ;;  insert1
    (setq node1 (rbnode3-parent node))
    (unless node1
      (return nil))

    ;;  insert2
    fixup-loop
    (setq node2 (rbnode3-parent node1))
    (when node2
      (setq left (rbnode3-left node2)))
    (multiple-value-setq (node check)
      (if (eq (rbnode3-left node1) node)
        (insert-left-rbnode3 node1)
        (insert-right-rbnode3 node1)))
    (unless node2
      (setf-rbnode3-black node)
      (setf (rbtree3-root inst) node)
      (return nil))
    (if (eq left node1)
      (setf (rbnode3-left node2) node)
      (setf (rbnode3-right node2) node))
    (setf (rbnode3-parent node) node2)
    (unless check
      (return nil))
    (setq node1 (rbnode3-parent node))
    (go fixup-loop)))


;;
;;  insert
;;
(defun replace-rbnode3 (node key value)
  (when *replace-key-rbtree3*
    (setf (rbnode3-key node) key))
  (setf (rbnode3-value node) value))

(defun insert-rbnode3 (compare node key value)
  (prog (key1 diff next)
    loop
    (setq key1 (rbnode3-key node))
    (setq diff (funcall compare key key1))

    ;; left
    (when (< diff 0)
      (setq next (rbnode3-left node))
      (unless next
        (setq next (make-rbnode3 :key key :value value))
        (setf (rbnode3-left node) next)
        (setf (rbnode3-parent next) node)
        (return next))
      (setq node next)
      (go loop))

    ;; right
    (when (< 0 diff)
      (setq next (rbnode3-right node))
      (unless next
        (setq next (make-rbnode3 :key key :value value))
        (setf (rbnode3-right node) next)
        (setf (rbnode3-parent next) node)
        (return next))
      (setq node next)
      (go loop))

    ;;  equal
    (when *replace-mode-rbtree3*
      (replace-rbnode3 node key value))
    (return nil)))

(defun insert2-rbtree3 (inst root key value)
  (let* ((compare (rbtree3-compare inst))
         (node (insert-rbnode3 compare root key value)))
    (when node
      (incf (rbtree3-size inst) 1)
      node)))

(defun insert1-rbtree3 (inst key value)
  (let ((node (make-rbnode3 :key key :value value)))
    (setf-rbnode3-black node)
    (setf (rbtree3-root inst) node)
    (setf (rbtree3-size inst) 1)
    node))

(defun insert0-rbtree3 (inst key value)
  (let ((root (rbtree3-root inst)))
    (if root
      (insert2-rbtree3 inst root key value)
      (insert1-rbtree3 inst key value))))

(defun insert-rbtree3
  (inst key value &optional (*replace-mode-rbtree3* *replace-mode-rbtree3*))
  (let ((node (insert0-rbtree3 inst key value)))
    (when node
      (insert-fixup-rbtree3 inst node)
      t)))

(defun intern-rbtree3 (inst key value)
  (declare (type rbtree3 inst))
  (insert-rbtree3 inst key value t))


;;
;;  search
;;
(defun search-rbnode3 (inst key)
  (declare (type rbtree3 inst))
  (prog ((node (rbtree3-root inst))
         (compare (rbtree3-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (rbnode3-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (rbnode3-left node)))
          ((< 0 diff) (setq node (rbnode3-right node)))
          (t (return node)))
    (go loop)))

(defun search-rbtree3 (inst key)
  (declare (type rbtree3 inst))
  (let ((node (search-rbnode3 inst key)))
    (if node
      (values (rbnode3-value node) t)
      (values nil nil))))

(defun replace-rbtree3
  (inst key value &optional (*replace-key-rbtree3* *replace-key-rbtree3*))
  (declare (type rbtree3 inst))
  (let ((node (search-rbnode3 inst key)))
    (when node
      (replace-rbnode3 node key value)
      t)))


;;
;;  delete fixup
;;
(defun delete-left-rbnode3 (node)
  (prog (left right right-left right-right)
    (setq right (rbnode3-right node))
    (setq right-left (rbnode3-left right))
    (when (rbnode3-red-p! right-left)
      (go red-label))
    (setq right-right (rbnode3-right right))
    (when (rbnode3-red-p! right-right)
      (go red-label))

    ;;  black - black
    (if (rbnode3-black-p right)
      (progn
        (setf-rbnode3-red right)
        (when (rbnode3-black-p node)
          (return (values node t)))
        (setf-rbnode3-black node)
        (return (values node nil)))
      (progn
        (setq node (rotate-left-rbnode3 node))
        (setq left (rbnode3-left node))
        (let ((next (delete-left-rbnode3 left)))
          (setf (rbnode3-left node) next)
          (setf (rbnode3-parent next) node))
        (return (values node nil))))

    ;;  red
    red-label
    (when (rbnode3-red-p! right-left)
      (let ((next (rotate-right-rbnode3 right)))
        (setf (rbnode3-right node) next)
        (setf (rbnode3-parent next) node)))
    (setq node (rotate-left-rbnode3 node))
    (setf-rbnode3-black (rbnode3-left node))
    (setf-rbnode3-black (rbnode3-right node))
    (return (values node nil))))

(defun delete-right-rbnode3 (node)
  (prog (left right left-left left-right)
    (setq left (rbnode3-left node))
    (setq left-right (rbnode3-right left))
    (when (rbnode3-red-p! left-right)
      (go red-label))
    (setq left-left (rbnode3-left left))
    (when (rbnode3-red-p! left-left)
      (go red-label))

    ;;  black - black
    (if (rbnode3-black-p left)
      (progn
        (setf-rbnode3-red left)
        (when (rbnode3-black-p node)
          (return (values node t)))
        (setf-rbnode3-black node)
        (return (values node nil)))
      (progn
        (setq node (rotate-right-rbnode3 node))
        (setq right (rbnode3-right node))
        (let ((next (delete-right-rbnode3 right)))
          (setf (rbnode3-right node) next)
          (setf (rbnode3-parent next) node))
        (return (values node nil))))

    ;;  red
    red-label
    (when (rbnode3-red-p! left-right)
      (let ((next (rotate-left-rbnode3 left)))
        (setf (rbnode3-left node) next)
        (setf (rbnode3-parent next) node)))
    (setq node (rotate-right-rbnode3 node))
    (setf-rbnode3-black (rbnode3-left node))
    (setf-rbnode3-black (rbnode3-right node))
    (return (values node nil))))

(defun delete-fixup-rbtree3 (inst node child direct delete)
  (prog (leftp left node1 next check)
    (when child
      (setf-rbnode3-black child)
      (return nil))
    (when (rbnode3-red-p delete)
      (return nil))
    (unless node
      (return nil))
    (setq leftp (eq direct 'left))

    fixup-loop
    (setq node1 (rbnode3-parent node))
    (when node1
      (setq left (rbnode3-left node1)))
    (multiple-value-setq (next check)
      (if leftp
        (delete-left-rbnode3 node)
        (delete-right-rbnode3 node)))
    (unless node1
      (setf-rbnode3-black next)
      (setf (rbtree3-root inst) next)
      (return nil))
    (if (eq left node)
      (setf (rbnode3-left node1) next leftp t)
      (setf (rbnode3-right node1) next leftp nil))
    (setf (rbnode3-parent next) node1)
    (unless check
      (return nil))
    (setq node node1)
    (go fixup-loop)))


;;
;;  delete
;;
(defun delete-copy-rbnode3 (replace delete)
  (setf (rbnode3-key replace) (rbnode3-key delete))
  (setf (rbnode3-value replace) (rbnode3-value delete)))

(defun delete-single-rbnode3 (inst delete child)
  (let ((parent (rbnode3-parent delete)) direct)
    (if parent
      (if (eq (rbnode3-left parent) delete)
        (setf (rbnode3-left parent) child direct 'left)
        (setf (rbnode3-right parent) child direct 'right))
      (setf (rbtree3-root inst) child direct nil))
    (when child
      (setf (rbnode3-parent child) parent))
    (delete-fixup-rbtree3 inst parent child direct delete)
    (free-rbnode3 delete)))

(defun delete-swap-rbnode3 (inst replace)
  (let* ((delete (next-rbnode3 replace))
         (right (rbnode3-right delete)))
    (delete-copy-rbnode3 replace delete)
    (delete-single-rbnode3 inst delete right)))

(defun delete-rbnode3 (inst delete)
  (let ((left (rbnode3-left delete))
        (right (rbnode3-right delete)))
    (if (and left right)
      (delete-swap-rbnode3 inst delete)
      (delete-single-rbnode3 inst delete (or left right)))))

(defun delete-rbtree3 (inst key)
  (declare (type rbtree3 inst))
  (let ((delete (search-rbnode3 inst key)))
    (when delete
      (delete-rbnode3 inst delete)
      (decf (rbtree3-size inst) 1)
      t)))


;;
;;  init
;;
(defstruct init-rbtree3-struct
  call depth limit lower direct)

(defun init-loop-rbtree3 (str now)
  (prog* ((call (init-rbtree3-struct-call str))
          (depth (init-rbtree3-struct-depth str))
          (limit (init-rbtree3-struct-limit str))
          (lower (init-rbtree3-struct-lower str))
          (direct (init-rbtree3-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value color)
    (setq color 'black)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (setq color 'red)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-rbtree3-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-rbtree3 str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-rbtree3 str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-rbnode3
                 :key key :value value :color color
                 :left left :right right))
    (when left
      (setf (rbnode3-parent left) node))
    (when right
      (setf (rbnode3-parent right) node))
    (return (values node (max x y)))))

(defun init-call-rbtree3 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-rbtree3
      (make-init-rbtree3-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-rbtree3 (call size &optional inst direct)
  (unless inst
    (setq inst (make-rbtree3)))
  (let ((root (init-call-rbtree3 call size direct)))
    (when root
      (setf-rbnode3-black root))
    (setf (rbtree3-root inst) root)
    (setf (rbtree3-size inst) size)
    inst))

(defun init-property1-rbtree3 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-rbtree3 (seq size)
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

(defun init-property-rbtree3 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-rbtree3
        (if (listp seq)
          (init-property1-rbtree3 seq)
          (init-property2-rbtree3 seq div))
        div inst))))

(defun init-associate1-rbtree3 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-rbtree3 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-rbtree3 (inst seq)
  (let ((size (length seq)))
    (init-rbtree3
      (if (listp seq)
        (init-associate1-rbtree3 seq)
        (init-associate2-rbtree3 seq size))
      size inst)))

