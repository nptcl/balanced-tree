#!/usr/bin/env -S sbcl --script

(defun load! (file)
  (if (probe-file file)
    (load file)
    (load (merge-pathnames file #p"../"))))

(load! #p"bintree1.lisp")
(load! #p"bintree2.lisp")
(load! #p"bintree3.lisp")
(load! #p"avltree1.lisp")
(load! #p"avltree2.lisp")
(load! #p"avltree3.lisp")
(load! #p"rbtree1.lisp")
(load! #p"rbtree2.lisp")
(load! #p"rbtree3.lisp")

(defpackage work
  (:use common-lisp
        bintree1 bintree2 bintree3
        avltree1 avltree2 avltree3
        rbtree1 rbtree2 rbtree3))
(in-package work)

;;
;;  array
;;
(defun random-swap (array size)
  (dotimes (x size)
    (let ((y (random size)))
      (unless (= x y)
        (rotatef (aref array x) (aref array y))))))

(defun make-random-left (array size)
  (loop for i from 0 below size
        do (setf (aref array i) i)))

(defun make-random-right (array size)
  (loop for i downfrom (1- size) downto 0
        do (setf (aref array i) i)))

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


;;
;;  main
;;
(defun main-insert (inst array call)
  (let ((size (length array)) v)
    (dotimes (i size)
      (setq v (aref array i))
      (funcall call inst v v))))

(defun main-delete (inst array call)
  (let ((size (length array)) v)
    (dotimes (i size)
      (setq v (aref array i))
      (funcall call inst v))))

(defun main ()
  (let* ((avl1 (make-avltree1))
         (avl2 (make-avltree2))
         (avl3 (make-avltree3))
         (rb1 (make-rbtree1))
         (rb2 (make-rbtree2))
         (rb3 (make-rbtree3))
         (size 1000000)
         (type 'random)
         (array (make-random-array size type)))
    (format t "avltree1~%")
    (time (main-insert avl1 array #'insert-avltree1))
    (time (main-delete avl1 array #'delete-avltree1))
    (format t "avltree2~%")
    (time (main-insert avl2 array #'insert-avltree2))
    (time (main-delete avl2 array #'delete-avltree2))
    (format t "avltree3~%")
    (time (main-insert avl3 array #'insert-avltree3))
    (time (main-delete avl3 array #'delete-avltree3))
    (format t "rbtree1~%")
    (time (main-insert rb1 array #'insert-rbtree1))
    (time (main-delete rb1 array #'delete-rbtree1))
    (format t "rbtree2~%")
    (time (main-insert rb2 array #'insert-rbtree2))
    (time (main-delete rb2 array #'delete-rbtree2))
    (format t "rbtree3~%")
    (time (main-insert rb3 array #'insert-rbtree3))
    (time (main-delete rb3 array #'delete-rbtree3))
    ))

(let ((*random-state* (make-random-state t)))
  (main))

