;;
;;  Red-Black Tree
;;
(defpackage #:rbtree1
  (:use common-lisp)
  (:export
    #:rbtree1
    #:rbtree1-p
    #:make-rbtree1
    #:clear-rbtree1
    #:size-rbtree1
    #:rbnode1-p
    #:key-rbnode1
    #:value-rbnode1

    #:empty-rbtree1
    #:min-rbtree1
    #:min-rbnode1
    #:max-rbtree1
    #:max-rbnode1
    #:map-rbtree1
    #:map-rbnode1
    #:keys-rbtree1
    #:values-rbtree1
    #:hash-table-rbtree1

    #:*replace-mode-rbtree1*
    #:*replace-key-rbtree1*
    #:insert-rbtree1
    #:intern-rbtree1
    #:search-rbtree1
    #:search-rbnode1
    #:replace-rbtree1
    #:delete-rbtree1
    #:init-rbtree1
    #:init-property-rbtree1
    #:init-associate-rbtree1))

(in-package #:rbtree1)

(defvar *replace-mode-rbtree1* nil)
(defvar *replace-key-rbtree1* t)


;;
;;  rbtree1
;;
(defstruct (rbtree1
             (:constructor rbtree1-heap)
             (:copier nil))
  root
  (compare #'- :type function)
  (size 0 :type unsigned-byte))

(defmethod print-object ((inst rbtree1) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (rbtree1-size inst))))

(defun make-rbtree1 (&key (compare #'-))
  (rbtree1-heap :compare compare))

(defun clear-rbtree1 (inst)
  (declare (type rbtree1 inst))
  (setf (rbtree1-root inst) nil)
  (setf (rbtree1-size inst) 0))

(defun size-rbtree1 (inst)
  (declare (type rbtree1 inst))
  (rbtree1-size inst))


;;
;;  rbnode1
;;
(defstruct (rbnode1 (:copier nil))
  left right key value
  (color 'red :type (member red black :error)))

(defmethod print-object ((inst rbnode1) stream)
  (print-unreadable-object (inst stream :identity t :type t)
    (format stream "~A" (rbnode1-color inst))))

(defun key-rbnode1 (inst)
  (declare (type rbnode1 inst))
  (rbnode1-key inst))

(defun value-rbnode1 (inst)
  (declare (type rbnode1 inst))
  (rbnode1-value inst))

(defun rbnode1-error-check (node)
  (declare (type (or rbnode1 null) node))
  (when (and node (eq (rbnode1-color node) :error))
    (error "rbnode1-color error, ~S." node))
  (values))

(defun rbnode1-red-p (node)
  (declare (type rbnode1 node))
  (rbnode1-error-check node)
  (eq (rbnode1-color node) 'red))

(defun rbnode1-black-p (node)
  (declare (type rbnode1 node))
  (rbnode1-error-check node)
  (eq (rbnode1-color node) 'black))

(defun rbnode1-red-p! (node)
  (rbnode1-error-check node)
  (and node
       (eq (rbnode1-color node) 'red)))

(defun rbnode1-black-p! (node)
  (rbnode1-error-check node)
  (or (null node)
      (eq (rbnode1-color node) 'black)))

(defun setf-rbnode1-red (node)
  (declare (type rbnode1 node))
  (rbnode1-error-check node)
  (setf (rbnode1-color node) 'red))

(defun setf-rbnode1-black (node)
  (declare (type rbnode1 node))
  (rbnode1-error-check node)
  (setf (rbnode1-color node) 'black))

(defun free-rbnode1 (delete)
  (setf (rbnode1-left delete) :error)
  (setf (rbnode1-right delete) :error)
  (setf (rbnode1-key delete) :error)
  (setf (rbnode1-value delete) :error)
  (setf (rbnode1-color delete) :error)
  nil)


;;
;;  operator
;;
(defun empty-rbtree1 (inst)
  (declare (type rbtree1 inst))
  (if (rbtree1-root inst) nil t))

;;  min
(defun min-node-rbnode1 (node)
  (let ((left (rbnode1-left node)))
    (if left
      (min-node-rbnode1 left)
      node)))

(defun min-rbnode1 (x)
  (etypecase x
    (rbnode1
      (min-node-rbnode1 x))
    (rbtree1
      (setq x (rbtree1-root x))
      (when x
        (min-node-rbnode1 x)))))

(defun min-rbtree1 (inst)
  (declare (type rbtree1 inst))
  (let ((node (min-rbnode1 inst)))
    (if node
      (values (rbnode1-key node) t)
      (values nil nil))))

;;  max
(defun max-node-rbnode1 (node)
  (let ((right (rbnode1-right node)))
    (if right
      (max-node-rbnode1 right)
      node)))

(defun max-rbnode1 (x)
  (etypecase x
    (rbnode1
      (max-node-rbnode1 x))
    (rbtree1
      (setq x (rbtree1-root x))
      (when x
        (max-node-rbnode1 x)))))

(defun max-rbtree1 (inst)
  (declare (type rbtree1 inst))
  (let ((node (max-rbnode1 inst)))
    (if node
      (values (rbnode1-key node) t)
      (values nil nil))))

;;  map
(defun map-rbnode1 (call inst)
  (declare (type rbtree1 inst))
  (labels ((rec (x) (when x
                      (rec (rbnode1-right x))
                      (funcall call x)
                      (rec (rbnode1-left x)))))
    (rec (rbtree1-root inst)))
  (values))

(defun map-rbtree1 (call inst)
  (declare (type rbtree1 inst))
  (map-rbnode1
    (lambda (x)
      (funcall call (rbnode1-key x) (rbnode1-value x)))
    inst))

(defun keys-rbtree1 (inst)
  (declare (type rbtree1 inst))
  (let (list)
    (map-rbnode1
      (lambda (x)
        (push (rbnode1-key x) list))
      inst)
    list))

(defun values-rbtree1 (inst)
  (declare (type rbtree1 inst))
  (let (list)
    (map-rbnode1
      (lambda (x)
        (push (rbnode1-value x) list))
      inst)
    list))

(defun hash-table-rbtree1 (inst &optional (test 'eql))
  (declare (type rbtree1 inst))
  (let ((table (make-hash-table :test test)))
    (map-rbtree1
      (lambda (key value)
        (setf (gethash key table) value))
      inst)
    table))


;;
;;  rotate
;;
(defun rotate-left-rbnode1 (node)
  (let ((x (rbnode1-right node)))
    (setf (rbnode1-right node) (rbnode1-left x))
    (setf (rbnode1-left x) node)
    (setf (rbnode1-color x) (rbnode1-color node))
    (setf-rbnode1-red node)
    x))

(defun rotate-right-rbnode1 (node)
  (let ((x (rbnode1-left node)))
    (setf (rbnode1-left node) (rbnode1-right x))
    (setf (rbnode1-right x) node)
    (setf (rbnode1-color x) (rbnode1-color node))
    (setf-rbnode1-red node)
    x))


;;
;;  insert
;;
(defun replace-rbnode1 (node key value)
  (when *replace-key-rbtree1*
    (setf (rbnode1-key node) key))
  (setf (rbnode1-value node) value))

(defun insert-left-rbnode1 (node check)
  (prog (left right next)
    (unless (eq check 'update)
      (return (values node check)))
    (when (rbnode1-red-p node)
      (return (values node 'update)))

    ;;  black
    (setq left (rbnode1-left node))
    (setq next (rbnode1-right left))
    (unless next
      (go next-check))
    (when (rbnode1-black-p next)
      (go next-check))
    (setq left (rotate-left-rbnode1 left))
    (setf (rbnode1-left node) left)

    next-check
    (setq next (rbnode1-left left))
    (unless next
      (go return-balanced))
    (when (rbnode1-black-p next)
      (go return-balanced))

    ;;  red-red
    (setq right (rbnode1-right node))
    (unless right
      (go rotate-right))
    (when (rbnode1-black-p right)
      (go rotate-right))
    (setf-rbnode1-red node)
    (setf-rbnode1-black left)
    (setf-rbnode1-black right)
    (return (values node 'update))

    rotate-right
    (setq node (rotate-right-rbnode1 node))
    return-balanced
    (return (values node 'balanced))))

(defun insert-right-rbnode1 (node check)
  (prog (left right next)
    (unless (eq check 'update)
      (return (values node check)))
    (when (rbnode1-red-p node)
      (return (values node 'update)))

    ;;  black
    (setq right (rbnode1-right node))
    (setq next (rbnode1-left right))
    (unless next
      (go next-check))
    (when (rbnode1-black-p next)
      (go next-check))
    (setq right (rotate-right-rbnode1 right))
    (setf (rbnode1-right node) right)

    next-check
    (setq next (rbnode1-right right))
    (unless next
      (go return-balanced))
    (when (rbnode1-black-p next)
      (go return-balanced))

    ;;  red-red
    (setq left (rbnode1-left node))
    (unless left
      (go rotate-left))
    (when (rbnode1-black-p left)
      (go rotate-left))
    (setf-rbnode1-red node)
    (setf-rbnode1-black left)
    (setf-rbnode1-black right)
    (return (values node 'update))

    rotate-left
    (setq node (rotate-left-rbnode1 node))
    return-balanced
    (return (values node 'balanced))))

(defun insert-rbnode1 (compare node key value)
  (prog (key1 diff next check)
    ;;  nil
    (unless node
      (setq next (make-rbnode1 :key key :value value))
      (return (values next 'update)))

    ;;  move
    (setq key1 (rbnode1-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))

    ;;  equal
    (when *replace-mode-rbtree1*
      (replace-rbnode1 node key value))
    (return (values nil nil))

    ;;  left
    move-left
    (setq next (rbnode1-left node))
    (multiple-value-setq (next check) (insert-rbnode1 compare next key value))
    (unless check
      (return (values nil nil)))
    (setf (rbnode1-left node) next)
    (return (insert-left-rbnode1 node check))

    ;;  right
    move-right
    (setq next (rbnode1-right node))
    (multiple-value-setq (next check) (insert-rbnode1 compare next key value))
    (unless check
      (return (values nil nil)))
    (setf (rbnode1-right node) next)
    (return (insert-right-rbnode1 node check))))

(defun insert-rbtree1
  (inst key value &optional (*replace-mode-rbtree1* *replace-mode-rbtree1*))
  (declare (type rbtree1 inst))
  (let ((compare (rbtree1-compare inst))
        (root (rbtree1-root inst)))
    (multiple-value-bind (node check) (insert-rbnode1 compare root key value)
      (when check
        (setf-rbnode1-black node)
        (setf (rbtree1-root inst) node)
        (incf (rbtree1-size inst) 1)
        t))))

(defun intern-rbtree1 (inst key value)
  (declare (type rbtree1 inst))
  (insert-rbtree1 inst key value t))


;;
;;  search
;;
(defun search-rbnode1 (inst key)
  (declare (type rbtree1 inst))
  (prog ((node (rbtree1-root inst))
         (compare (rbtree1-compare inst))
         key1 diff)
    loop
    (unless node
      (return nil))
    (setq key1 (rbnode1-key node))
    (setq diff (funcall compare key key1))
    (cond ((< diff 0) (setq node (rbnode1-left node)))
          ((< 0 diff) (setq node (rbnode1-right node)))
          (t (return node)))
    (go loop)))

(defun search-rbtree1 (inst key)
  (declare (type rbtree1 inst))
  (let ((node (search-rbnode1 inst key)))
    (if node
      (values (rbnode1-value node) t)
      (values nil nil))))

(defun replace-rbtree1
  (inst key value &optional (*replace-key-rbtree1* *replace-key-rbtree1*))
  (declare (type rbtree1 inst))
  (let ((node (search-rbnode1 inst key)))
    (when node
      (replace-rbnode1 node key value)
      t)))


;;
;;  delete
;;
(defun delete-copy-rbnode1 (replace delete)
  (setf (rbnode1-key replace) (rbnode1-key delete))
  (setf (rbnode1-value replace) (rbnode1-value delete))
  (free-rbnode1 delete))

(defun delete-left-rbnode1 (node check)
  (prog (left right right-left right-right)
    (when (eq check 'balanced)
      (return (values node 'balanced)))
    (setq right (rbnode1-right node))
    (setq right-left (rbnode1-left right))
    (when (rbnode1-red-p! right-left)
      (go red-label))
    (setq right-right (rbnode1-right right))
    (when (rbnode1-red-p! right-right)
      (go red-label))

    ;;  black - black
    (if (rbnode1-black-p right)
      (progn
        (setf-rbnode1-red right)
        (when (rbnode1-black-p node)
          (return (values node 'update)))
        (setf-rbnode1-black node)
        (return (values node 'balanced)))
      (progn
        (setq node (rotate-left-rbnode1 node))
        (setq left (rbnode1-left node))
        (setf (rbnode1-left node) (delete-left-rbnode1 left 'update))
        (return (values node 'balanced))))

    ;;  red
    red-label
    (when (rbnode1-red-p! right-left)
      (setf (rbnode1-right node) (rotate-right-rbnode1 right)))
    (setq node (rotate-left-rbnode1 node))
    (setf-rbnode1-black (rbnode1-left node))
    (setf-rbnode1-black (rbnode1-right node))
    (return (values node 'balanced))))

(defun delete-right-rbnode1 (node check)
  (prog (left right left-left left-right)
    (when (eq check 'balanced)
      (return (values node 'balanced)))
    (setq left (rbnode1-left node))
    (setq left-right (rbnode1-right left))
    (when (rbnode1-red-p! left-right)
      (go red-label))
    (setq left-left (rbnode1-left left))
    (when (rbnode1-red-p! left-left)
      (go red-label))

    ;;  black - black
    (if (rbnode1-black-p left)
      (progn
        (setf-rbnode1-red left)
        (when (rbnode1-black-p node)
          (return (values node 'update)))
        (setf-rbnode1-black node)
        (return (values node 'balanced)))
      (progn
        (setq node (rotate-right-rbnode1 node))
        (setq right (rbnode1-right node))
        (setf (rbnode1-right node) (delete-right-rbnode1 right 'update))
        (return (values node 'balanced))))

    ;;  red
    red-label
    (when (rbnode1-red-p! left-right)
      (setf (rbnode1-left node) (rotate-left-rbnode1 left)))
    (setq node (rotate-right-rbnode1 node))
    (setf-rbnode1-black (rbnode1-left node))
    (setf-rbnode1-black (rbnode1-right node))
    (return (values node 'balanced))))

(defun delete-swap-rbnode1 (replace node)
  (prog (next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  left
    (setq next (rbnode1-left node))
    (multiple-value-setq (next check) (delete-swap-rbnode1 replace next))
    (when check
      (setf (rbnode1-left node) next)
      (return (delete-left-rbnode1 node check)))

    ;;  right
    (setq next (rbnode1-right node))
    (when next
      (setf-rbnode1-black next)
      (delete-copy-rbnode1 replace node)
      (return (values next 'balanced)))

    ;;  single
    (if (rbnode1-red-p node)
      (setq check 'balanced)
      (setq check 'update))
    (delete-copy-rbnode1 replace node)
    (return (values nil check))))

(defun delete-rbnode1 (compare node key)
  (prog (key1 diff left right next check)
    ;;  nil
    (unless node
      (return (values nil nil)))

    ;;  move
    (setq key1 (rbnode1-key node))
    (setq diff (funcall compare key key1))
    (when (< diff 0)
      (go move-left))
    (when (< 0 diff)
      (go move-right))
    (go move-delete)

    ;;  left
    move-left
    (setq next (rbnode1-left node))
    (multiple-value-setq (next check) (delete-rbnode1 compare next key))
    (unless check
      (return (values nil nil)))
    (setf (rbnode1-left node) next)
    (return (delete-left-rbnode1 node check))

    ;;  right
    move-right
    (setq next (rbnode1-right node))
    (multiple-value-setq (next check) (delete-rbnode1 compare next key))
    (unless check
      (return (values nil nil)))
    (setf (rbnode1-right node) next)
    (return (delete-right-rbnode1 node check))

    ;;  delete
    move-delete
    (setq left (rbnode1-left node))
    (setq right (rbnode1-right node))
    (when (and left right)
      (go delete-swap))
    (when left
      (go delete-left))
    (when right
      (go delete-right))
    (go delete-single)

    ;;  left, right
    delete-swap
    (multiple-value-setq (next check) (delete-swap-rbnode1 node right))
    (setf (rbnode1-right node) next)
    (return (delete-right-rbnode1 node check))

    ;;  left only
    delete-left
    (setf-rbnode1-black left)
    (free-rbnode1 node)
    (return (values left 'balanced))

    ;;  right only
    delete-right
    (setf-rbnode1-black right)
    (free-rbnode1 node)
    (return (values right 'balanced))

    ;;  null, null
    delete-single
    (if (rbnode1-red-p node)
      (setq check 'balanced)
      (setq check 'update))
    (free-rbnode1 node)
    (return (values nil check))))

(defun delete-rbtree1 (inst key)
  (declare (type rbtree1 inst))
  (let ((compare (rbtree1-compare inst))
        (root (rbtree1-root inst)))
    (multiple-value-bind (node check) (delete-rbnode1 compare root key)
      (when check
        (setf (rbtree1-root inst) node)
        (decf (rbtree1-size inst) 1)
        t))))


;;
;;  init
;;
(defstruct init-rbtree1-struct
  call depth limit lower direct)

(defun init-loop-rbtree1 (str now)
  (prog* ((call (init-rbtree1-struct-call str))
          (depth (init-rbtree1-struct-depth str))
          (limit (init-rbtree1-struct-limit str))
          (lower (init-rbtree1-struct-lower str))
          (direct (init-rbtree1-struct-direct str))
          (depth1 (1- depth))
          node left right x y key value color)
    (setq color 'black)
    (unless (< now depth)
      (return (values nil now)))
    (when (= now depth1)
      (setq color 'red)
      (unless (< lower limit)
        (return (values nil now)))
      (incf (init-rbtree1-struct-lower str) 1))
    (incf now 1)
    (multiple-value-setq (left x) (init-loop-rbtree1 str now))
    (multiple-value-setq (key value) (funcall call))
    (multiple-value-setq (right y) (init-loop-rbtree1 str now))
    (when direct
      (rotatef left right)
      (rotatef x y))
    (setq node (make-rbnode1
                 :key key :value value :color color
                 :left left :right right))
    (return (values node (max x y)))))

(defun init-call-rbtree1 (call size direct)
  (let* ((depth (integer-length size))
         (limit (- size (1- (ash 1 (1- depth))))))
    (init-loop-rbtree1
      (make-init-rbtree1-struct
        :call call :depth depth :limit limit :lower 0 :direct direct)
      0)))

(defun init-rbtree1 (call size &optional inst direct)
  (unless inst
    (setq inst (make-rbtree1)))
  (let ((root (init-call-rbtree1 call size direct)))
    (when root
      (setf-rbnode1-black root))
    (setf (rbtree1-root inst) root)
    (setf (rbtree1-size inst) size)
    inst))

(defun init-property1-rbtree1 (seq)
  (lambda (&aux key value)
    (unless seq
      (error "sequence error, key."))
    (setq key (pop seq))
    (unless seq
      (error "sequence error, value, ~S." key))
    (setq value (pop seq))
    (values key value)))

(defun init-property2-rbtree1 (seq size)
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

(defun init-property-rbtree1 (inst seq)
  (let ((size (length seq)))
    (multiple-value-bind (div rem) (truncate size 2)
      (unless (zerop rem)
        (error "size error, ~A." size))
      (init-rbtree1
        (if (listp seq)
          (init-property1-rbtree1 seq)
          (init-property2-rbtree1 seq div))
        div inst))))

(defun init-associate1-rbtree1 (seq)
  (lambda ()
    (unless seq
      (error "sequence error, cons."))
    (destructuring-bind (key . value) (pop seq)
      (values key value))))

(defun init-associate2-rbtree1 (seq size)
  (let ((index 0))
    (lambda ()
      (unless (< index size)
        (error "sequence error, cons."))
      (destructuring-bind (key . value) (aref seq index)
        (incf index 1)
        (values key value)))))

(defun init-associate-rbtree1 (inst seq)
  (let ((size (length seq)))
    (init-rbtree1
      (if (listp seq)
        (init-associate1-rbtree1 seq)
        (init-associate2-rbtree1 seq size))
      size inst)))

