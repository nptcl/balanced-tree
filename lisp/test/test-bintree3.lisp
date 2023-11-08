(load #p"bintree3.lisp")
(load #p"test/bintree3-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp bintree3 #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest bintree3.1
  (let ((inst (make-bintree3)))
    (check-bintree3-error inst)
    (values
      (empty-bintree3 inst)
      (size-bintree3 inst)))
  t 0)

(deftest bintree3.2
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 20)
    (check-bintree3-error inst)
    (search-bintree3 inst 10))
  20 t)

(deftest bintree3.3
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 20)
    (check-bintree3-error inst)
    (search-bintree3 inst 20))
  nil nil)

(deftest bintree3.4
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 20)
    (check-bintree3-error inst)
    (values
      (empty-bintree3 inst)
      (size-bintree3 inst)))
  nil 1)

(deftest bintree3.5
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 :aaa)
    (insert-bintree3 inst 20 :bbb)
    (check-bintree3-error inst)
    (values
      (empty-bintree3 inst)
      (size-bintree3 inst)))
  nil 2)

(deftest bintree3.6
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 :aaa)
    (insert-bintree3 inst 20 :bbb)
    (check-bintree3-error inst)
    (values
      (search-bintree3 inst 10)
      (search-bintree3 inst 20)
      (search-bintree3 inst 30)))
  :aaa :bbb nil)

(deftest bintree3.7
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 20 :bbb)
    (insert-bintree3 inst 10 :aaa)
    (check-bintree3-error inst)
    (values
      (search-bintree3 inst 10)
      (search-bintree3 inst 20)
      (search-bintree3 inst 30)))
  :aaa :bbb nil)

(deftest bintree3.8
  (let ((inst (make-bintree3)))
    (dotimes (i 20)
      (insert-bintree3 inst i (* 100 i))
      (check-bintree3-error inst))
    (values
      (search-bintree3 inst 5)
      (search-bintree3 inst 6)
      (search-bintree3 inst 100)
      (size-bintree3 inst)))
  500 600 nil 20)

(deftest bintree3.9
  (let ((inst (make-bintree3)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-bintree3 inst i (* 100 i))))
    (check-bintree3-error inst)
    (values
      (search-bintree3 inst 5)
      (search-bintree3 inst 6)
      (search-bintree3 inst 100)
      (size-bintree3 inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-bintree3 (constantly 10) 0)))
    (check-bintree3-error inst)
    (bintree3-p inst))
  t)

(defun init-bintree3-left (size)
  (let ((index 0))
    (init-bintree3
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-bintree3-right (size)
  (let ((index size))
    (init-bintree3
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-bintree3-insert (size)
  (let ((inst (make-bintree3)))
    (dotimes (index size inst)
      (insert-bintree3 inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-bintree3-left 1)))
    (check-bintree3-error inst)
    (bintree3-p inst))
  t)

(deftest init.3
  (let ((inst (init-bintree3-right 1)))
    (check-bintree3-error inst)
    (bintree3-p inst))
  t)

(deftest init.4
  (let ((inst (init-bintree3-left 1)))
    (check-bintree3-error inst)
    (values
      (size-bintree3 inst)
      (search-bintree3 inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-bintree3-right 1)))
    (check-bintree3-error inst)
    (values
      (size-bintree3 inst)
      (search-bintree3 inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-bintree3-left 2)))
    (check-bintree3-error inst)
    (values
      (size-bintree3 inst)
      (search-bintree3 inst 0)
      (search-bintree3 inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-bintree3-right 2)))
    (check-bintree3-error inst)
    (values
      (size-bintree3 inst)
      (search-bintree3 inst 0)
      (search-bintree3 inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-bintree3-left 2))
         (root (bintree3::bintree3-root inst)))
    (check-bintree3-error inst)
    (values
      (bintree3::binnode3-key
        (bintree3::binnode3-left root))
      (bintree3::binnode3-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-bintree3-right 2))
         (root (bintree3::bintree3-root inst)))
    (check-bintree3-error inst)
    (values
      (bintree3::binnode3-key root)
      (bintree3::binnode3-key
        (bintree3::binnode3-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-bintree3)))
    (prog1
      (delete-bintree3 inst 10)
      (check-bintree3-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-bintree3)) ret)
    (insert-bintree3 inst 10 20)
    (setq ret (delete-bintree3 inst 10))
    (check-bintree3-error inst)
    (values ret (size-bintree3 inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-bintree3)) ret)
    (insert-bintree3 inst 10 20)
    (setq ret (delete-bintree3 inst 20))
    (check-bintree3-error inst)
    (values ret (size-bintree3 inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-bintree3-left 2)) ret size root key value)
    (setq ret (delete-bintree3 inst 1))
    (check-bintree3-error inst)
    (setq size (size-bintree3 inst))
    (setq root (bintree3::bintree3-root inst))
    (setq key (bintree3::binnode3-key root))
    (setq value (bintree3::binnode3-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-bintree3-right 2)) ret size root key value)
    (setq ret (delete-bintree3 inst 0))
    (check-bintree3-error inst)
    (setq size (size-bintree3 inst))
    (setq root (bintree3::bintree3-root inst))
    (setq key (bintree3::binnode3-key root))
    (setq value (bintree3::binnode3-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-bintree3-left 2)) ret size root key value)
    (setq ret (delete-bintree3 inst 0))
    (setq size (size-bintree3 inst))
    (setq root (bintree3::bintree3-root inst))
    (setq key (bintree3::binnode3-key root))
    (setq value (bintree3::binnode3-value root))
    (check-bintree3-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-bintree3-right 2)) ret size root key value)
    (setq ret (delete-bintree3 inst 1))
    (setq size (size-bintree3 inst))
    (setq root (bintree3::bintree3-root inst))
    (setq key (bintree3::binnode3-key root))
    (setq value (bintree3::binnode3-value root))
    (check-bintree3-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-bintree3-left 2)))
    (delete-bintree3 inst 1)
    (check-bintree3-error inst)
    (delete-bintree3 inst 0)
    (check-bintree3-error inst)
    (size-bintree3 inst))
  0)

(deftest delete2.6
  (let ((inst (init-bintree3-left 2)))
    (delete-bintree3 inst 0)
    (check-bintree3-error inst)
    (delete-bintree3 inst 1)
    (check-bintree3-error inst)
    (size-bintree3 inst))
  0)

(deftest delete2.7
  (let ((inst (init-bintree3-right 2)))
    (delete-bintree3 inst 1)
    (check-bintree3-error inst)
    (delete-bintree3 inst 0)
    (check-bintree3-error inst)
    (size-bintree3 inst))
  0)

(deftest delete2.8
  (let ((inst (init-bintree3-right 2)))
    (delete-bintree3 inst 0)
    (check-bintree3-error inst)
    (delete-bintree3 inst 1)
    (check-bintree3-error inst)
    (size-bintree3 inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-bintree3-left 3)))
    (check-bintree3-error inst)
    (delete-bintree3 inst 0)
    (check-bintree3-error inst)
    (size-bintree3 inst))
  2)

(deftest delete3.2
  (let ((inst (init-bintree3-left 3)))
    (check-bintree3-error inst)
    (delete-bintree3 inst 2)
    (check-bintree3-error inst)
    (size-bintree3 inst))
  2)

(deftest delete3.3
  (let ((inst (init-bintree3-left 3)))
    (check-bintree3-error inst)
    (delete-bintree3 inst 1)
    (check-bintree3-error inst)
    (size-bintree3 inst))
  2)

(deftest delete3.4
  (let ((inst (init-bintree3-left 3)))
    (check-bintree3-error inst)
    (delete-bintree3 inst 1)
    (check-bintree3-error inst)
    (size-bintree3 inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-bintree3 (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-bintree3 inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-bintree3-left 4)))
    (check-bintree3-error inst)
    (delete-bintree3 inst 0)
    (check-bintree3-error inst)
    (values
      (size-bintree3 inst)
      (search-bintree3 inst 0)
      (search-check-bintree3 inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-bintree3-left 4)))
    (check-bintree3-error inst)
    (delete-bintree3 inst 1)
    (check-bintree3-error inst)
    (values
      (size-bintree3 inst)
      (search-bintree3 inst 1)
      (search-check-bintree3 inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-bintree3-left 4)))
    (check-bintree3-error inst)
    (delete-bintree3 inst 2)
    (check-bintree3-error inst)
    (values
      (size-bintree3 inst)
      (search-bintree3 inst 2)
      (search-check-bintree3 inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-bintree3-left 4)))
    (check-bintree3-error inst)
    (delete-bintree3 inst 3)
    (check-bintree3-error inst)
    (values
      (size-bintree3 inst)
      (search-bintree3 inst 3)
      (search-check-bintree3 inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-bintree3-left size))
                (right (init-bintree3-right size))
                (insert (init-bintree3-insert size)))))
    (check-bintree3-error inst)
    (delete-bintree3 inst index)
    (check-bintree3-error inst)
    (unless (= (1- size) (size-bintree3 inst))
      (error "size error."))
    (when (search-bintree3 inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-bintree3 inst list)
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
(deftest bintree3-delete.1
  (let ((inst (make-bintree3)))
    (prog1
      (delete-bintree3 inst 10)
      (check-bintree3-error inst)))
  nil)

(deftest bintree3-delete.2
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 20)
    (prog1
      (delete-bintree3 inst 10)
      (check-bintree3-error inst)))
  t)

(deftest bintree3-delete.3
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 20)
    (delete-bintree3 inst 10)
    (check-bintree3-error inst)
    (values
      (empty-bintree3 inst)
      (size-bintree3 inst)))
  t 0)

(deftest bintree3-delete.4
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 20)
    (delete-bintree3 inst 30)
    (check-bintree3-error inst)
    (values
      (empty-bintree3 inst)
      (size-bintree3 inst)))
  nil 1)

(deftest bintree3-delete.5
  (let ((inst (make-bintree3)))
    (dotimes (i 20)
      (insert-bintree3 inst i (* 100 i)))
    (delete-bintree3 inst 5)
    (delete-bintree3 inst 6)
    (delete-bintree3 inst 7)
    (check-bintree3-error inst)
    (values
      (search-bintree3 inst 4)
      (search-bintree3 inst 6)
      (search-bintree3 inst 100)
      (size-bintree3 inst)))
  400 nil nil 17)

(deftest bintree3-delete.6
  (let ((inst (make-bintree3)))
    (dotimes (i 20)
      (insert-bintree3 inst i (* 100 i)))
    (dotimes (i 20)
      (delete-bintree3 inst i))
    (values
      (empty-bintree3 inst)
      (size-bintree3 inst)))
  t 0)

(deftest bintree3-min.1
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 100)
    (insert-bintree3 inst 20 200)
    (insert-bintree3 inst 30 300)
    (min-bintree3 inst))
  10 t)

(deftest bintree3-min.2
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 100)
    (insert-bintree3 inst 20 200)
    (insert-bintree3 inst 30 300)
    (let ((x (min-binnode3 inst)))
      (values (binnode3-p x)
              (key-binnode3 x)
              (value-binnode3 x))))
  t 10 100)

(deftest bintree3-min.3
  (let ((inst (make-bintree3)))
    (insert-bintree3 inst 10 100)
    (insert-bintree3 inst 20 200)
    (insert-bintree3 inst 30 300)
    (let* ((x (min-binnode3 inst))
           (y (next-binnode3 x)))
      (values (binnode3-p y)
              (key-binnode3 y)
              (value-binnode3 y))))
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
  (let ((inst (make-bintree3))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-bintree3 inst vector)
    (check-bintree3-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-bintree3)))
    (dotimes (i +size+)
      (insert-bintree3 inst i (* 100 i)))
    (check-bintree3-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

