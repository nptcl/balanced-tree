(load #p"bintree2.lisp")
(load #p"test/bintree2-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp bintree2 #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest bintree2.1
  (let ((inst (make-bintree2)))
    (check-bintree2-error inst)
    (values
      (empty-bintree2 inst)
      (size-bintree2 inst)))
  t 0)

(deftest bintree2.2
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 20)
    (check-bintree2-error inst)
    (search-bintree2 inst 10))
  20 t)

(deftest bintree2.3
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 20)
    (check-bintree2-error inst)
    (search-bintree2 inst 20))
  nil nil)

(deftest bintree2.4
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 20)
    (check-bintree2-error inst)
    (values
      (empty-bintree2 inst)
      (size-bintree2 inst)))
  nil 1)

(deftest bintree2.5
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 :aaa)
    (insert-bintree2 inst 20 :bbb)
    (check-bintree2-error inst)
    (values
      (empty-bintree2 inst)
      (size-bintree2 inst)))
  nil 2)

(deftest bintree2.6
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 :aaa)
    (insert-bintree2 inst 20 :bbb)
    (check-bintree2-error inst)
    (values
      (search-bintree2 inst 10)
      (search-bintree2 inst 20)
      (search-bintree2 inst 30)))
  :aaa :bbb nil)

(deftest bintree2.7
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 20 :bbb)
    (insert-bintree2 inst 10 :aaa)
    (check-bintree2-error inst)
    (values
      (search-bintree2 inst 10)
      (search-bintree2 inst 20)
      (search-bintree2 inst 30)))
  :aaa :bbb nil)

(deftest bintree2.8
  (let ((inst (make-bintree2)))
    (dotimes (i 20)
      (insert-bintree2 inst i (* 100 i))
      (check-bintree2-error inst))
    (values
      (search-bintree2 inst 5)
      (search-bintree2 inst 6)
      (search-bintree2 inst 100)
      (size-bintree2 inst)))
  500 600 nil 20)

(deftest bintree2.9
  (let ((inst (make-bintree2)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-bintree2 inst i (* 100 i))))
    (check-bintree2-error inst)
    (values
      (search-bintree2 inst 5)
      (search-bintree2 inst 6)
      (search-bintree2 inst 100)
      (size-bintree2 inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-bintree2 (constantly 10) 0)))
    (check-bintree2-error inst)
    (bintree2-p inst))
  t)

(defun init-bintree2-left (size)
  (let ((index 0))
    (init-bintree2
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-bintree2-right (size)
  (let ((index size))
    (init-bintree2
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-bintree2-insert (size)
  (let ((inst (make-bintree2)))
    (dotimes (index size inst)
      (insert-bintree2 inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-bintree2-left 1)))
    (check-bintree2-error inst)
    (bintree2-p inst))
  t)

(deftest init.3
  (let ((inst (init-bintree2-right 1)))
    (check-bintree2-error inst)
    (bintree2-p inst))
  t)

(deftest init.4
  (let ((inst (init-bintree2-left 1)))
    (check-bintree2-error inst)
    (values
      (size-bintree2 inst)
      (search-bintree2 inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-bintree2-right 1)))
    (check-bintree2-error inst)
    (values
      (size-bintree2 inst)
      (search-bintree2 inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-bintree2-left 2)))
    (check-bintree2-error inst)
    (values
      (size-bintree2 inst)
      (search-bintree2 inst 0)
      (search-bintree2 inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-bintree2-right 2)))
    (check-bintree2-error inst)
    (values
      (size-bintree2 inst)
      (search-bintree2 inst 0)
      (search-bintree2 inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-bintree2-left 2))
         (root (bintree2::bintree2-root inst)))
    (check-bintree2-error inst)
    (values
      (bintree2::binnode2-key
        (bintree2::binnode2-left root))
      (bintree2::binnode2-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-bintree2-right 2))
         (root (bintree2::bintree2-root inst)))
    (check-bintree2-error inst)
    (values
      (bintree2::binnode2-key root)
      (bintree2::binnode2-key
        (bintree2::binnode2-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-bintree2)))
    (prog1
      (delete-bintree2 inst 10)
      (check-bintree2-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-bintree2)) ret)
    (insert-bintree2 inst 10 20)
    (setq ret (delete-bintree2 inst 10))
    (check-bintree2-error inst)
    (values ret (size-bintree2 inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-bintree2)) ret)
    (insert-bintree2 inst 10 20)
    (setq ret (delete-bintree2 inst 20))
    (check-bintree2-error inst)
    (values ret (size-bintree2 inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-bintree2-left 2)) ret size root key value)
    (setq ret (delete-bintree2 inst 1))
    (check-bintree2-error inst)
    (setq size (size-bintree2 inst))
    (setq root (bintree2::bintree2-root inst))
    (setq key (bintree2::binnode2-key root))
    (setq value (bintree2::binnode2-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-bintree2-right 2)) ret size root key value)
    (setq ret (delete-bintree2 inst 0))
    (check-bintree2-error inst)
    (setq size (size-bintree2 inst))
    (setq root (bintree2::bintree2-root inst))
    (setq key (bintree2::binnode2-key root))
    (setq value (bintree2::binnode2-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-bintree2-left 2)) ret size root key value)
    (setq ret (delete-bintree2 inst 0))
    (setq size (size-bintree2 inst))
    (setq root (bintree2::bintree2-root inst))
    (setq key (bintree2::binnode2-key root))
    (setq value (bintree2::binnode2-value root))
    (check-bintree2-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-bintree2-right 2)) ret size root key value)
    (setq ret (delete-bintree2 inst 1))
    (setq size (size-bintree2 inst))
    (setq root (bintree2::bintree2-root inst))
    (setq key (bintree2::binnode2-key root))
    (setq value (bintree2::binnode2-value root))
    (check-bintree2-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-bintree2-left 2)))
    (delete-bintree2 inst 1)
    (check-bintree2-error inst)
    (delete-bintree2 inst 0)
    (check-bintree2-error inst)
    (size-bintree2 inst))
  0)

(deftest delete2.6
  (let ((inst (init-bintree2-left 2)))
    (delete-bintree2 inst 0)
    (check-bintree2-error inst)
    (delete-bintree2 inst 1)
    (check-bintree2-error inst)
    (size-bintree2 inst))
  0)

(deftest delete2.7
  (let ((inst (init-bintree2-right 2)))
    (delete-bintree2 inst 1)
    (check-bintree2-error inst)
    (delete-bintree2 inst 0)
    (check-bintree2-error inst)
    (size-bintree2 inst))
  0)

(deftest delete2.8
  (let ((inst (init-bintree2-right 2)))
    (delete-bintree2 inst 0)
    (check-bintree2-error inst)
    (delete-bintree2 inst 1)
    (check-bintree2-error inst)
    (size-bintree2 inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-bintree2-left 3)))
    (check-bintree2-error inst)
    (delete-bintree2 inst 0)
    (check-bintree2-error inst)
    (size-bintree2 inst))
  2)

(deftest delete3.2
  (let ((inst (init-bintree2-left 3)))
    (check-bintree2-error inst)
    (delete-bintree2 inst 2)
    (check-bintree2-error inst)
    (size-bintree2 inst))
  2)

(deftest delete3.3
  (let ((inst (init-bintree2-left 3)))
    (check-bintree2-error inst)
    (delete-bintree2 inst 1)
    (check-bintree2-error inst)
    (size-bintree2 inst))
  2)

(deftest delete3.4
  (let ((inst (init-bintree2-left 3)))
    (check-bintree2-error inst)
    (delete-bintree2 inst 1)
    (check-bintree2-error inst)
    (size-bintree2 inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-bintree2 (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-bintree2 inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-bintree2-left 4)))
    (check-bintree2-error inst)
    (delete-bintree2 inst 0)
    (check-bintree2-error inst)
    (values
      (size-bintree2 inst)
      (search-bintree2 inst 0)
      (search-check-bintree2 inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-bintree2-left 4)))
    (check-bintree2-error inst)
    (delete-bintree2 inst 1)
    (check-bintree2-error inst)
    (values
      (size-bintree2 inst)
      (search-bintree2 inst 1)
      (search-check-bintree2 inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-bintree2-left 4)))
    (check-bintree2-error inst)
    (delete-bintree2 inst 2)
    (check-bintree2-error inst)
    (values
      (size-bintree2 inst)
      (search-bintree2 inst 2)
      (search-check-bintree2 inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-bintree2-left 4)))
    (check-bintree2-error inst)
    (delete-bintree2 inst 3)
    (check-bintree2-error inst)
    (values
      (size-bintree2 inst)
      (search-bintree2 inst 3)
      (search-check-bintree2 inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-bintree2-left size))
                (right (init-bintree2-right size))
                (insert (init-bintree2-insert size)))))
    (check-bintree2-error inst)
    (delete-bintree2 inst index)
    (check-bintree2-error inst)
    (unless (= (1- size) (size-bintree2 inst))
      (error "size error."))
    (when (search-bintree2 inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-bintree2 inst list)
        (error "search-check error.")))))

(defun delete-all-type (direct size)
  (dotimes (i size)
    (delete-all-index direct size i)))

(defun delete-all (size)
  (delete-all-type 'left size)
  (delete-all-type 'right size)
  (delete-all-type 'insert size)
  (values))

(deftest delete4.all
  (delete-all 4))

(deftest delete5.all
  (delete-all 5))

(deftest delete6.all
  (delete-all 6))

(deftest delete7.all
  (delete-all 7))

(deftest delete8.all
  (delete-all 8))

(deftest delete.many.all
  (dotimes (i 65)
    (delete-all i))
  nil)


;;  delete
(deftest bintree2-delete.1
  (let ((inst (make-bintree2)))
    (prog1
      (delete-bintree2 inst 10)
      (check-bintree2-error inst)))
  nil)

(deftest bintree2-delete.2
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 20)
    (prog1
      (delete-bintree2 inst 10)
      (check-bintree2-error inst)))
  t)

(deftest bintree2-delete.3
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 20)
    (delete-bintree2 inst 10)
    (check-bintree2-error inst)
    (values
      (empty-bintree2 inst)
      (size-bintree2 inst)))
  t 0)

(deftest bintree2-delete.4
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 20)
    (delete-bintree2 inst 30)
    (check-bintree2-error inst)
    (values
      (empty-bintree2 inst)
      (size-bintree2 inst)))
  nil 1)

(deftest bintree2-delete.5
  (let ((inst (make-bintree2)))
    (dotimes (i 20)
      (insert-bintree2 inst i (* 100 i)))
    (delete-bintree2 inst 5)
    (delete-bintree2 inst 6)
    (delete-bintree2 inst 7)
    (check-bintree2-error inst)
    (values
      (search-bintree2 inst 4)
      (search-bintree2 inst 6)
      (search-bintree2 inst 100)
      (size-bintree2 inst)))
  400 nil nil 17)

(deftest bintree2-delete.6
  (let ((inst (make-bintree2)))
    (dotimes (i 20)
      (insert-bintree2 inst i (* 100 i)))
    (dotimes (i 20)
      (delete-bintree2 inst i))
    (values
      (empty-bintree2 inst)
      (size-bintree2 inst)))
  t 0)

(deftest bintree2-min.1
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 100)
    (insert-bintree2 inst 20 200)
    (insert-bintree2 inst 30 300)
    (min-bintree2 inst))
  10 t)

(deftest bintree2-min.2
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 100)
    (insert-bintree2 inst 20 200)
    (insert-bintree2 inst 30 300)
    (let ((x (min-binnode2 inst)))
      (values (binnode2-p x)
              (key-binnode2 x)
              (value-binnode2 x))))
  t 10 100)

(deftest bintree2-min.3
  (let ((inst (make-bintree2)))
    (insert-bintree2 inst 10 100)
    (insert-bintree2 inst 20 200)
    (insert-bintree2 inst 30 300)
    (let* ((x (min-binnode2 inst))
           (y (next-binnode2 x)))
      (values (binnode2-p y)
              (key-binnode2 y)
              (value-binnode2 y))))
  t 20 200)


;;
;;  do-tests
;;
(do-tests)
(fresh-line)


;;
;;  init
;;
(defconstant +size+ 1000000)

(defun main-init ()
  (let ((inst (make-bintree2))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-bintree2 inst vector)
    (check-bintree2-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-bintree2)))
    (dotimes (i +size+)
      (insert-bintree2 inst i (* 100 i)))
    (check-bintree2-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

