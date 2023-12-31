(cl:in-package common-lisp-user)
(load #p"avltree1.lisp")
(load #p"avltree2.lisp")
(load #p"avltree3.lisp")
(load #p"bintree1.lisp")
(load #p"bintree2.lisp")
(load #p"bintree3.lisp")
(load #p"rbtree1.lisp")
(load #p"rbtree2.lisp")
(load #p"rbtree3.lisp")
(load #p"test/avltree1-check.lisp")
(load #p"test/avltree2-check.lisp")
(load #p"test/avltree3-check.lisp")
(load #p"test/bintree1-check.lisp")
(load #p"test/bintree2-check.lisp")
(load #p"test/bintree3-check.lisp")
(load #p"test/rbtree1-check.lisp")
(load #p"test/rbtree2-check.lisp")
(load #p"test/rbtree3-check.lisp")

(defpackage work (:use common-lisp
                       bintree1 bintree2 bintree3
                       avltree1 avltree2 avltree3
                       rbtree1 rbtree2 rbtree3))
(in-package work)

(defparameter *size* 100)

;;
;;  interface
;;
(defclass calltree () ())
(defgeneric clear-calltree (inst))
(defgeneric insert-calltree (inst key value))
(defgeneric delete-calltree (inst key))
(defgeneric search-calltree (inst key))
(defgeneric map-calltree (call inst))
(defgeneric check-calltree-error (inst))

;;  clear
(defmethod clear-calltree ((inst bintree1))
  (clear-bintree1 inst))
(defmethod clear-calltree ((inst bintree2))
  (clear-bintree2 inst))
(defmethod clear-calltree ((inst bintree3))
  (clear-bintree3 inst))
(defmethod clear-calltree ((inst avltree1))
  (clear-avltree1 inst))
(defmethod clear-calltree ((inst avltree2))
  (clear-avltree2 inst))
(defmethod clear-calltree ((inst avltree3))
  (clear-avltree3 inst))
(defmethod clear-calltree ((inst rbtree1))
  (clear-rbtree1 inst))
(defmethod clear-calltree ((inst rbtree2))
  (clear-rbtree2 inst))
(defmethod clear-calltree ((inst rbtree3))
  (clear-rbtree3 inst))

;;  insert
(defmethod insert-calltree ((inst bintree1) key value)
  (insert-bintree1 inst key value))
(defmethod insert-calltree ((inst bintree2) key value)
  (insert-bintree2 inst key value))
(defmethod insert-calltree ((inst bintree3) key value)
  (insert-bintree3 inst key value))
(defmethod insert-calltree ((inst avltree1) key value)
  (insert-avltree1 inst key value))
(defmethod insert-calltree ((inst avltree2) key value)
  (insert-avltree2 inst key value))
(defmethod insert-calltree ((inst avltree3) key value)
  (insert-avltree3 inst key value))
(defmethod insert-calltree ((inst rbtree1) key value)
  (insert-rbtree1 inst key value))
(defmethod insert-calltree ((inst rbtree2) key value)
  (insert-rbtree2 inst key value))
(defmethod insert-calltree ((inst rbtree3) key value)
  (insert-rbtree3 inst key value))

;;  delete
(defmethod delete-calltree ((inst bintree1) key)
  (delete-bintree1 inst key))
(defmethod delete-calltree ((inst bintree2) key)
  (delete-bintree2 inst key))
(defmethod delete-calltree ((inst bintree3) key)
  (delete-bintree3 inst key))
(defmethod delete-calltree ((inst avltree1) key)
  (delete-avltree1 inst key))
(defmethod delete-calltree ((inst avltree2) key)
  (delete-avltree2 inst key))
(defmethod delete-calltree ((inst avltree3) key)
  (delete-avltree3 inst key))
(defmethod delete-calltree ((inst rbtree1) key)
  (delete-rbtree1 inst key))
(defmethod delete-calltree ((inst rbtree2) key)
  (delete-rbtree2 inst key))
(defmethod delete-calltree ((inst rbtree3) key)
  (delete-rbtree3 inst key))

;;  search
(defmethod search-calltree ((inst bintree1) key)
  (search-bintree1 inst key))
(defmethod search-calltree ((inst bintree2) key)
  (search-bintree2 inst key))
(defmethod search-calltree ((inst bintree3) key)
  (search-bintree3 inst key))
(defmethod search-calltree ((inst avltree1) key)
  (search-avltree1 inst key))
(defmethod search-calltree ((inst avltree2) key)
  (search-avltree2 inst key))
(defmethod search-calltree ((inst avltree3) key)
  (search-avltree3 inst key))
(defmethod search-calltree ((inst rbtree1) key)
  (search-rbtree1 inst key))
(defmethod search-calltree ((inst rbtree2) key)
  (search-rbtree2 inst key))
(defmethod search-calltree ((inst rbtree3) key)
  (search-rbtree3 inst key))

;;  map
(defmethod map-calltree (call (inst bintree1))
  (map-bintree1 call inst))
(defmethod map-calltree (call (inst bintree2))
  (map-bintree2 call inst))
(defmethod map-calltree (call (inst bintree3))
  (map-bintree3 call inst))
(defmethod map-calltree (call (inst avltree1))
  (map-avltree1 call inst))
(defmethod map-calltree (call (inst avltree2))
  (map-avltree2 call inst))
(defmethod map-calltree (call (inst avltree3))
  (map-avltree3 call inst))
(defmethod map-calltree (call (inst rbtree1))
  (map-rbtree1 call inst))
(defmethod map-calltree (call (inst rbtree2))
  (map-rbtree2 call inst))
(defmethod map-calltree (call (inst rbtree3))
  (map-rbtree3 call inst))

;;  check
(defmethod check-calltree-error ((inst bintree1))
  (check-bintree1-error inst))
(defmethod check-calltree-error ((inst bintree2))
  (check-bintree2-error inst))
(defmethod check-calltree-error ((inst bintree3))
  (check-bintree3-error inst))
(defmethod check-calltree-error ((inst avltree1))
  (check-avltree1-error inst))
(defmethod check-calltree-error ((inst avltree2))
  (check-avltree2-error inst))
(defmethod check-calltree-error ((inst avltree3))
  (check-avltree3-error inst))
(defmethod check-calltree-error ((inst rbtree1))
  (check-rbtree1-error inst))
(defmethod check-calltree-error ((inst rbtree2))
  (check-rbtree2-error inst))
(defmethod check-calltree-error ((inst rbtree3))
  (check-rbtree3-error inst))


;;
;;  main
;;
(defun equal-key-value (inst)
  (let (list)
    (map-calltree
      (lambda (key value)
        (push key list)
        (push value list))
      inst)
    (nreverse list)))

(defun equal-tree (x y)
  (equal (equal-key-value x)
         (equal-key-value y)))

(defun equal-tree-error (x y)
  (unless (equal-tree x y)
    (error "equal-tree error, ~S, ~S." x y)))

(defun insert-testcall (size x y insert)
  (dotimes (i size)
    (destructuring-bind (key . value) (aref insert i)
      (insert-calltree x key value)
      (insert-calltree y key value)
      (check-calltree-error x)
      (check-calltree-error y)
      (equal-tree-error x y))))

(defun delete-testcall (size x y delete)
  (dotimes (i size)
    (destructuring-bind (key . value) (aref delete i)
      (insert-calltree x key value)
      (insert-calltree y key value)
      (check-calltree-error x)
      (check-calltree-error y)
      (equal-tree-error x y))))

(defun insert-delete-testcall (size x y insert delete)
  (insert-testcall size x y insert)
  (delete-testcall size x y delete))

(defun random-swap (array size)
  (dotimes (x size)
    (let ((y (random size)))
      (unless (= x y)
        (rotatef (aref array x) (aref array y))))))

(defun make-random-left (array size)
  (loop for i from 0 below size
        do (setf (aref array i) (cons i (* i 100)))))

(defun make-random-right (array size)
  (loop for i downfrom (1- size) downto 0
        do (setf (aref array i) (cons i (* i 100)))))

(defun make-random-randomly (array size)
  (make-random-left array size)
  (dotimes (i 10)
    (random-swap array size)))

(defun make-random-array (size type)
  (let ((array (make-array size)))
    (ecase type
      (left (make-random-left array size))
      (right (make-random-right array size))
      (random (make-random-randomly array size)))
    array))

(defun cross-product (list1 list2)
  (let (list)
    (dolist (x list1)
      (dolist (y list2)
        (push (cons x y) list)))
    (nreverse list)))

;;  insert0
(defun insert0-delete-random (size left right)
  (let ((x (make-bintree1))
        (y (make-avltree1))
        (z (make-rbtree1))
        (array1 (make-random-array size left))
        (array2 (make-random-array size right)))
    (insert-delete-testcall size x y array1 array2)
    (clear-calltree x)
    (insert-delete-testcall size x z array1 array2)
    (clear-calltree x)))

(defun main-loop0 (index)
  (let ((size *size*)
        (list '(left right random)))
    (dolist (x (cross-product list list))
      (destructuring-bind (left . right) x
        (format t "Test.~A: ~A, ~A, ~A~%" index size left right)
        (insert0-delete-random size left right)))))

;;  insert1
(defun insert1-delete-random (size left right)
  (let ((x (make-bintree2))
        (y (make-avltree2))
        (z (make-rbtree2))
        (array1 (make-random-array size left))
        (array2 (make-random-array size right)))
    (insert-delete-testcall size x y array1 array2)
    (clear-calltree x)
    (insert-delete-testcall size x z array1 array2)
    (clear-calltree x)))

(defun main-loop1 (index)
  (let ((size *size*)
        (list '(left right random)))
    (dolist (x (cross-product list list))
      (destructuring-bind (left . right) x
        (format t "Test-Parent.~A: ~A, ~A, ~A~%" index size left right)
        (insert1-delete-random size left right)))))

;;  insert2
(defun insert2-delete-random (size left right)
  (let ((x (make-bintree3))
        (y (make-avltree3))
        (z (make-rbtree3))
        (array1 (make-random-array size left))
        (array2 (make-random-array size right)))
    (insert-delete-testcall size x y array1 array2)
    (clear-calltree x)
    (insert-delete-testcall size x z array1 array2)
    (clear-calltree x)))

(defun main-loop2 (index)
  (let ((size *size*)
        (list '(left right random)))
    (dolist (x (cross-product list list))
      (destructuring-bind (left . right) x
        (format t "Test-Parent.~A: ~A, ~A, ~A~%" index size left right)
        (insert2-delete-random size left right)))))

;;  main-loop
(defun main-loop (index)
  (main-loop0 index)
  (main-loop1 index)
  (main-loop2 index))

(defun main-call ()
  (dotimes (i 10)
    (main-loop i)))

(defun main ()
  (let ((*random-state* (make-random-state t)))
    (main-call)))
(main)

