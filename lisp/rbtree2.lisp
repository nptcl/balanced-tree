;;
;;  Red-Black Tree  (parent)
;;
(defpackage #:rbtree2
  (:use common-lisp)
  (:export
    #:rbtree2
    #:rbtree2-p
    #:make-rbtree2
    #:clear-rbtree2
    #:size-rbtree2
    #:rbnode2-p
    #:key-rbnode2
    #:value-rbnode2

    #:empty-rbtree2
    #:min-rbtree2
    #:min-rbnode2
    #:max-rbtree2
    #:max-rbnode2
    #:map-rbtree2
    #:map-rbnode2
    #:keys-rbtree2
    #:values-rbtree2
    #:hash-table-rbtree2
    #:next-rbnode2
    #:prev-rbnode2

    #:*replace-mode-rbtree2*
    #:*replace-key-rbtree2*
    #:insert-rbtree2
    #:intern-rbtree2
    #:search-rbtree2
    #:search-rbnode2
    #:replace-rbtree2
    #:delete-rbtree2
    #:init-rbtree2
    #:init-property-rbtree2
    #:init-associate-rbtree2))

(in-package #:rbtree2)

(defvar *replace-mode-rbtree2* nil)
(defvar *replace-key-rbtree2* t)


;;
;;  rbtree2
;;
(defstruct (rbtree2
             (:constructor rbtree2-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst rbtree2) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (rbtree2-size inst))))

(defun make-rbtree2 (&key (compare #'-))
  (rbtree2-heap :compare compare))

(defun clear-rbtree2 (inst)
  (declare (type rbtree2 inst))
  (setf (rbtree2-root inst) nil)
  (setf (rbtree2-size inst) 0))

(defun size-rbtree2 (inst)
  (declare (type rbtree2 inst))
  (rbtree2-size inst))


;;
;;  rbnode2
;;
(defstruct (rbnode2 (:copier nil))
  parent left right key value
  (color 'red :type (member red black :error)))

(defmethod print-object ((inst rbnode2) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (rbnode2-color inst))))

(defun key-rbnode2 (inst)
  (declare (type rbnode2 inst))
  (rbnode2-key inst))

(defun value-rbnode2 (inst)
  (declare (type rbnode2 inst))
  (rbnode2-value inst))

(defun rbnode2-error-check (node)
  (declare (type (or rbnode2 null) node))
  (when (and node (eq (rbnode2-color node) :error))
    (error "rbnode2-color error, ~S." node))
  (values))

(defun rbnode2-red-p (node)
  (declare (type rbnode2 node))
  (rbnode2-error-check node)
  (eq (rbnode2-color node) 'red))

(defun rbnode2-black-p (node)
  (declare (type rbnode2 node))
  (rbnode2-error-check node)
  (eq (rbnode2-color node) 'black))

(defun rbnode2-red-p! (node)
  (rbnode2-error-check node)
  (and node
       (eq (rbnode2-color node) 'red)))

(defun rbnode2-black-p! (node)
  (rbnode2-error-check node)
  (or (null node)
      (eq (rbnode2-color node) 'black)))

(defun setf-rbnode2-red (node)
  (declare (type rbnode2 node))
  (rbnode2-error-check node)
  (setf (rbnode2-color node) 'red))

(defun setf-rbnode2-black (node)
  (declare (type rbnode2 node))
  (rbnode2-error-check node)
  (setf (rbnode2-color node) 'black))

(defun free-rbnode2 (delete)
  (setf (rbnode2-parent delete) :error)
  (setf (rbnode2-left delete) :error)
  (setf (rbnode2-right delete) :error)
  (setf (rbnode2-key delete) :error)
  (setf (rbnode2-value delete) :error)
  (setf (rbnode2-color delete) :error)
  nil)


;;
;;  operator
;;
(defun empty-rbtree2 (inst)
  (declare (type rbtree2 inst))
  (if (rbtree2-root inst) nil t))

;;  min
(defun min-node-rbnode2 (node)
  (let ((left (rbnode2-left node)))
    (if left
      (min-node-rbnode2 left)
      node)))

(defun min-rbnode2 (x)
  (etypecase x
    (rbnode2
      (min-node-rbnode2 x))
    (rbtree2
      (setq x (rbtree2-root x))
      (when x
        (min-node-rbnode2 x)))))

(defun min-rbtree2 (inst)
  (declare (type rbtree2 inst))
  (let ((node (min-rbnode2 inst)))
    (if node
      (values (rbnode2-key node) t)
      (values nil nil))))

;;  max
(defun max-node-rbnode2 (node)
  (let ((right (rbnode2-right node)))
    (if right
      (max-node-rbnode2 right)
      node)))

(defun max-rbnode2 (x)
  (etypecase x
    (rbnode2
      (max-node-rbnode2 x))
    (rbtree2
      (setq x (rbtree2-root x))
      (when x
        (max-node-rbnode2 x)))))

(defun max-rbtree2 (inst)
  (declare (type rbtree2 inst))
  (let ((node (max-rbnode2 inst)))
    (if node
      (values (rbnode2-key node) t)
      (values nil nil))))

;;  map
(defun map-rbnode2 (call inst)
  (declare (type rbtree2 inst))
  (labels ((rec (x) (when x
                      (rec (rbnode2-right x))
                      (funcall call x)
                      (rec (rbnode2-left x)))))
    (rec (rbtree2-root inst)))
  (values))

(defun map-rbtree2 (call inst)
  (declare (type rbtree2 inst))
  (map-rbnode2
    (lambda (x)
      (funcall call (rbnode2-key x) (rbnode2-value x)))
    inst))

(defun keys-rbtree2 (inst)
  (declare (type rbtree2 inst))
  (let (list)
    (map-rbnode2
      (lambda (x)
        (push (rbnode2-key x) list))
      inst)
    list))

(defun values-rbtree2 (inst)
  (declare (type rbtree2 inst))
  (let (list)
    (map-rbnode2
      (lambda (x)
        (push (rbnode2-value x) list))
      inst)
    list))

(defun hash-table-rbtree2 (inst &optional (test 'eql))
  (declare (type rbtree2 inst))
  (let ((table (make-hash-table :test test)))
    (map-rbtree2
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))

;;  parent
(defun next-rbnode2 (x)
  (prog (y z)
    (setq y (rbnode2-right x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (rbnode2-left y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (rbnode2-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (rbnode2-right y))
    (unless (eq x z)
      (return y))
    (setq x y y (rbnode2-parent y))
    (go parent-loop)))

(defun prev-rbnode2 (x)
  (prog (y z)
    (setq y (rbnode2-left x))
    (unless y
      (go parent-find))

    lastnode-loop
    (setq x (rbnode2-right y))
    (unless x
      (return y))
    (setq y x)
    (go lastnode-loop)

    parent-find
    (setq y (rbnode2-parent x))
    parent-loop
    (unless y
      (return nil))
    (setq z (rbnode2-left y))
    (unless (eq x z)
      (return y))
    (setq x y y (rbnode2-parent y))
    (go parent-loop)))


;;
;;  rotate
;;
(defun rotate-left-rbnode2 (node)
  (let* ((p (rbnode2-parent node))
         (leftp (and p (eq (rbnode2-left p) node)))
         (x (rbnode2-right node))
         (y (rbnode2-left x)))
    (setf (rbnode2-right node) y)
    (when y
      (setf (rbnode2-parent y) node))
    (setf (rbnode2-left x) node)
    (setf (rbnode2-parent node) x)
    (if (null p)
      (setf (rbnode2-parent x) nil)
      (if leftp
        (setf (rbnode2-left p) x)
        (setf (rbnode2-right p) x)))
    (setf (rbnode2-color x) (rbnode2-color node))
    (setf-rbnode2-red node)
    x))

(defun rotate-right-rbnode2 (node)
  (let* ((p (rbnode2-parent node))
         (leftp (and p (eq (rbnode2-left p) node)))
         (x (rbnode2-left node))
         (y (rbnode2-right x)))
    (setf (rbnode2-left node) y)
    (when y
      (setf (rbnode2-parent y) node))
    (setf (rbnode2-right x) node)
    (setf (rbnode2-parent node) x)
    (if (null p)
      (setf (rbnode2-parent x) nil)
      (if leftp
        (setf (rbnode2-left p) x)
        (setf (rbnode2-right p) x)))
    (setf (rbnode2-color x) (rbnode2-color node))
    (setf-rbnode2-red node)
    x))


;;
;;  insert
;;
(defun replace-rbnode2 (node key value)
  (when *replace-key-rbtree2*
    (setf (rbnode2-key node) key))
  (setf (rbnode2-value node) value))

(defun insert-left-rbnode2 (node check)
  (prog (left right next)
    (unless (eq check 'update)
      (return (values node check)))
    (when (rbnode2-red-p node)
      (return (values node 'update)))

    ;;  black
    (setq left (rbnode2-left node))
    (setq next (rbnode2-right left))
    (unless next
      (go next-check))
    (when (rbnode2-black-p next)
      (go next-check))
    (setq left (rotate-left-rbnode2 left))
    (setf (rbnode2-left node) left)
    (setf (rbnode2-parent left) node)

    next-check
    (setq next (rbnode2-left left))
    (unless next
      (go return-balanced))
    (when (rbnode2-black-p next)
      (go return-balanced))

    ;;  red-red
    (setq right (rbnode2-right node))
    (unless right
      (go rotate-right))
    (when (rbnode2-black-p right)
      (go rotate-right))
    (setf-rbnode2-red node)
    (setf-rbnode2-black left)
    (setf-rbnode2-black right)
    (return (values node 'update))

    rotate-right
    (setq node (rotate-right-rbnode2 node))
    return-balanced
    (return (values node 'balanced))))

(defun insert-right-rbnode2 (node check)
  (prog (left right next)
    (unless (eq check 'update)
      (return (values node check)))
    (when (rbnode2-red-p node)
      (return (values node 'update)))

    ;;  black
    (setq right (rbnode2-right node))
    (setq next (rbnode2-left right))
    (unless next
      (go next-check))
    (when (rbnode2-black-p next)
      (go next-check))
    (setq right (rotate-right-rbnode2 right))
    (setf (rbnode2-right node) right)
    (setf (rbnode2-parent right) node)

    next-check
    (setq next (rbnode2-right right))
    (unless next
      (go return-balanced))
    (when (rbnode2-black-p next)
      (go return-balanced))

    ;;  red-red
    (setq left (rbnode2-left node))
    (unless left
      (go rotate-left))
    (when (rbnode2-black-p left)
      (go rotate-left))
    (setf-rbnode2-red node)
    (setf-rbnode2-black left)
    (setf-rbnode2-black right)
    (return (values node 'update))

    rotate-left
    (setq node (rotate-left-rbnode2 node))
    return-balanced
    (return (values node 'balanced))))

(defun insert-rbnode2 (compare node key value)
  (prog (key1 diff next check)
    ;;  nil
    (unless node
      (setq next (make-rbnode2 :key key :value value))
      (return (values next 'update)))

    ;;  move
    (setq key1 (rbnode2-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))

    ;;  equal
    (when *replace-mode-rbtree2*
      (replace-rbnode2 node key value))
    (return (values nil nil))

    ;;  left
    move-left
    (setq next (rbnode2-left node))
    (multiple-value-setq (next check) (insert-rbnode2 compare next key value))
    (unless check
      (return (values nil nil)))
    (setf (rbnode2-left node) next)
    (setf (rbnode2-parent next) node)
    (return (insert-left-rbnode2 node check))

    ;;  right
    move-right
    (setq next (rbnode2-right node))
    (multiple-value-setq (next check) (insert-rbnode2 compare next key value))
    (unless check
      (return (values nil nil)))
    (setf (rbnode2-right node) next)
    (setf (rbnode2-parent next) node)
    (return (insert-right-rbnode2 node check))))

(defun insert-rbtree2
  (inst key value &optional (*replace-mode-rbtree2* *replace-mode-rbtree2*))
  (declare (type rbtree2 inst))
  (let ((compare (rbtree2-compare inst))
        (root (rbtree2-root inst)))
    (multiple-value-bind (node check) (insert-rbnode2 compare root key value)
      (when check
        (setf-rbnode2-black node)
        (setf (rbtree2-root inst) node)
        (incf (rbtree2-size inst) 1)
        t))))

(defun intern-rbtree2 (inst key value)
  (declare (type rbtree2 inst))
  (insert-rbtree2 inst key value t))


;;
;;  search
;;
(defun search-rbnode2 (inst key)
  (declare (type rbtree2 inst))
  (prog ((node (rbtree2-root inst))
         (compare (rbtree2-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (rbnode2-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (rbnode2-left node)))
          ((< 0 diff) (setq node (rbnode2-right node)))
          (t (return node)))
    (go loop)))

(defun search-rbtree2 (inst key)
  (declare (type rbtree2 inst))
  (let ((node (search-rbnode2 inst key)))
    (if node
      (values (rbnode2-value node) t)
      (values nil nil))))

(defun replace-rbtree2
  (inst key value &optional (*replace-key-rbtree2* *replace-key-rbtree2*))
  (declare (type rbtree2 inst))
  (let ((node (search-rbnode2 inst key)))
    (when node
      (replace-rbnode2 node key value)
      t)))


;;
;;  delete
;;
(defun delete-copy-rbnode2 (replace delete)
  (setf (rbnode2-key replace) (rbnode2-key delete))
  (setf (rbnode2-value replace) (rbnode2-value delete))
  (free-rbnode2 delete))

(defun delete-left-rbnode2 (node check)
  (prog (left right right-left right-right)
    (when (eq check 'balanced)
      (return (values node 'balanced)))
    (setq right (rbnode2-right node))
    (setq right-left (rbnode2-left right))
    (when (rbnode2-red-p! right-left)
      (go red-label))
    (setq right-right (rbnode2-right right))
    (when (rbnode2-red-p! right-right)
      (go red-label))

    ;;  black - black
    (if (rbnode2-black-p right)
      (progn
        (setf-rbnode2-red right)
        (when (rbnode2-black-p node)
          (return (values node 'update)))
        (setf-rbnode2-black node)
        (return (values node 'balanced)))
      (progn
        (setq node (rotate-left-rbnode2 node))
        (setq left (rbnode2-left node))
        (let ((next (delete-left-rbnode2 left 'update)))
          (setf (rbnode2-left node) next)
          (setf (rbnode2-parent next) node))
        (return (values node 'balanced))))

    ;;  red
    red-label
    (when (rbnode2-red-p! right-left)
      (let ((next (rotate-right-rbnode2 right)))
        (setf (rbnode2-right node) next)
        (setf (rbnode2-parent next) node)))
    (setq node (rotate-left-rbnode2 node))
    (setf-rbnode2-black (rbnode2-left node))
    (setf-rbnode2-black (rbnode2-right node))
    (return (values node 'balanced))))

(defun delete-right-rbnode2 (node check)
  (prog (left right left-left left-right)
    (when (eq check 'balanced)
      (return (values node 'balanced)))
    (setq left (rbnode2-left node))
    (setq left-right (rbnode2-right left))
    (when (rbnode2-red-p! left-right)
      (go red-label))
    (setq left-left (rbnode2-left left))
    (when (rbnode2-red-p! left-left)
      (go red-label))

    ;;  black - black
    (if (rbnode2-black-p left)
      (progn
        (setf-rbnode2-red left)
        (when (rbnode2-black-p node)
          (return (values node 'update)))
        (setf-rbnode2-black node)
        (return (values node 'balanced)))
      (progn
        (setq node (rotate-right-rbnode2 node))
        (setq right (rbnode2-right node))
        (let ((next (delete-right-rbnode2 right 'update)))
          (setf (rbnode2-right node) next)
          (setf (rbnode2-parent next) node))
        (return (values node 'balanced))))

    ;;  red
    red-label
    (when (rbnode2-red-p! left-right)
      (let ((next (rotate-left-rbnode2 left)))
        (setf (rbnode2-left node) next)
        (setf (rbnode2-parent next) node)))
    (setq node (rotate-right-rbnode2 node))
    (setf-rbnode2-black (rbnode2-left node))
    (setf-rbnode2-black (rbnode2-right node))
    (return (values node 'balanced))))

(defun delete-swap-rbnode2 (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  left
    (setq next (rbnode2-left node))
    (multiple-value-setq (next check) (delete-swap-rbnode2 replace next))
    (when check
      (setf (rbnode2-left node) next)
      (when next
        (setf (rbnode2-parent next) node))
      (return (delete-left-rbnode2 node check)))

    ;;  right
    (setq next (rbnode2-right node))
    (when next
      (setf-rbnode2-black next)
      (delete-copy-rbnode2 replace node)
      (return (values next 'balanced)))

    ;;  single
    (if (rbnode2-red-p node)
      (setq check 'balanced)
      (setq check 'update))
    (delete-copy-rbnode2 replace node)
    (return (values nil check))))

(defun delete-rbnode2 (compare node key)
  (prog (key1 diff left right next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (rbnode2-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))
    (go move-delete)

    ;;  left
    move-left
    (setq next (rbnode2-left node))
    (multiple-value-setq (next check) (delete-rbnode2 compare next key))
    (unless check
      (return (values nil nil)))
    (setf (rbnode2-left node) next)
    (when next
      (setf (rbnode2-parent next) node))
    (return (delete-left-rbnode2 node check))

    ;;  right
    move-right
    (setq next (rbnode2-right node))
    (multiple-value-setq (next check) (delete-rbnode2 compare next key))
    (unless check
      (return (values nil nil)))
    (setf (rbnode2-right node) next)
    (when next
      (setf (rbnode2-parent next) node))
    (return (delete-right-rbnode2 node check))

    ;;  delete
    move-delete
    (setq left (rbnode2-left node))
    (setq right (rbnode2-right node))
    (when (and left right)
      (go delete-swap))
    (when left
      (go delete-left))
    (when right
      (go delete-right))
    (go delete-single)

    ;;  left, right
    delete-swap
    (multiple-value-setq (next check) (delete-swap-rbnode2 node right))
    (setf (rbnode2-right node) next)
    (when next
      (setf (rbnode2-parent next) node))
    (return (delete-right-rbnode2 node check))

    ;;  left only
    delete-left
    (setf-rbnode2-black left)
    (free-rbnode2 node)
    (return (values left 'balanced))

    ;;  right only
    delete-right
    (setf-rbnode2-black right)
    (free-rbnode2 node)
    (return (values right 'balanced))

    ;;  null, null
    delete-single
    (if (rbnode2-red-p node)
      (setq check 'balanced)
      (setq check 'update))
    (free-rbnode2 node)
    (return (values nil check))))

(defun delete-rbtree2 (inst key)
  (declare (type rbtree2 inst))
  (let ((compare (rbtree2-compare inst))
        (root (rbtree2-root inst)))
    (multiple-value-bind (node check) (delete-rbnode2 compare root key)
      (when check
        (setf (rbtree2-root inst) node)
        (when node
          (setf (rbnode2-parent node) nil))
        (decf (rbtree2-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-rbtree2-struct
  call depth limit lower direct)

(defun init-loop-rbtree2 (str now)
  (prog* ((call (init-rbtree2-struct-call str))
          (depth (init-rbtree2-struct-depth str))
          (limit (init-rbtree2-struct-limit str))
          (lower (init-rbtree2-struct-lower str))
          (direct (init-rbtree2-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value color)
    (setq color 'black)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (setq color 'red)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-rbtree2-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-rbtree2 str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-rbtree2 str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-rbnode2
                 :key key :value value :color color
                 :left left :right right))
    (when left
      (setf (rbnode2-parent left) node))
    (when right
      (setf (rbnode2-parent right) node))
    (return (values node (max x y)))))

(defun init-call-rbtree2 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-rbtree2
      (make-init-rbtree2-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-rbtree2 (call size &optional inst direct)
  (unless inst
    (setq inst (make-rbtree2)))
  (let ((root (init-call-rbtree2 call size direct)))
    (when root
      (setf-rbnode2-black root))
    (setf (rbtree2-root inst) root)
    (setf (rbtree2-size inst) size)
    inst))

(defun init-property1-rbtree2 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-rbtree2 (seq size)
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

(defun init-property-rbtree2 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-rbtree2
        (if (listp seq)
          (init-property1-rbtree2 seq)
          (init-property2-rbtree2 seq div))
        div inst))))

(defun init-associate1-rbtree2 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-rbtree2 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-rbtree2 (inst seq)
  (let ((size (length seq)))
    (init-rbtree2
      (if (listp seq)
        (init-associate1-rbtree2 seq)
        (init-associate2-rbtree2 seq size))
      size inst)))

