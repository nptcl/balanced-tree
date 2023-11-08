(load #p"avltree2.lisp")
(load #p"test/avltree2-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp avltree2 #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest avltree2.1
  (let ((inst (make-avltree2)))
    (check-avltree2-error inst)
    (values
      (empty-avltree2 inst)
      (size-avltree2 inst)))
  t 0)

(deftest avltree2.2
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 20)
    (check-avltree2-error inst)
    (search-avltree2 inst 10))
  20 t)

(deftest avltree2.3
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 20)
    (check-avltree2-error inst)
    (search-avltree2 inst 20))
  nil nil)

(deftest avltree2.4
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 20)
    (check-avltree2-error inst)
    (values
      (empty-avltree2 inst)
      (size-avltree2 inst)))
  nil 1)

(deftest avltree2.5
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 :aaa)
    (insert-avltree2 inst 20 :bbb)
    (check-avltree2-error inst)
    (values
      (empty-avltree2 inst)
      (size-avltree2 inst)))
  nil 2)

(deftest avltree2.6
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 :aaa)
    (insert-avltree2 inst 20 :bbb)
    (check-avltree2-error inst)
    (values
      (search-avltree2 inst 10)
      (search-avltree2 inst 20)
      (search-avltree2 inst 30)))
  :aaa :bbb nil)

(deftest avltree2.7
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 20 :bbb)
    (insert-avltree2 inst 10 :aaa)
    (check-avltree2-error inst)
    (values
      (search-avltree2 inst 10)
      (search-avltree2 inst 20)
      (search-avltree2 inst 30)))
  :aaa :bbb nil)

(deftest avltree2.8
  (let ((inst (make-avltree2)))
    (dotimes (i 20)
      (insert-avltree2 inst i (* 100 i))
      (check-avltree2-error inst))
    (values
      (search-avltree2 inst 5)
      (search-avltree2 inst 6)
      (search-avltree2 inst 100)
      (size-avltree2 inst)))
  500 600 nil 20)

(deftest avltree2.9
  (let ((inst (make-avltree2)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-avltree2 inst i (* 100 i))))
    (check-avltree2-error inst)
    (values
      (search-avltree2 inst 5)
      (search-avltree2 inst 6)
      (search-avltree2 inst 100)
      (size-avltree2 inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-avltree2 (constantly 10) 0)))
    (check-avltree2-error inst)
    (avltree2-p inst))
  t)

(defun init-avltree2-left (size)
  (let ((index 0))
    (init-avltree2
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-avltree2-right (size)
  (let ((index size))
    (init-avltree2
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-avltree2-insert (size)
  (let ((inst (make-avltree2)))
    (dotimes (index size inst)
      (insert-avltree2 inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-avltree2-left 1)))
    (check-avltree2-error inst)
    (avltree2-p inst))
  t)

(deftest init.3
  (let ((inst (init-avltree2-right 1)))
    (check-avltree2-error inst)
    (avltree2-p inst))
  t)

(deftest init.4
  (let ((inst (init-avltree2-left 1)))
    (check-avltree2-error inst)
    (values
      (size-avltree2 inst)
      (search-avltree2 inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-avltree2-right 1)))
    (check-avltree2-error inst)
    (values
      (size-avltree2 inst)
      (search-avltree2 inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-avltree2-left 2)))
    (check-avltree2-error inst)
    (values
      (size-avltree2 inst)
      (search-avltree2 inst 0)
      (search-avltree2 inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-avltree2-right 2)))
    (check-avltree2-error inst)
    (values
      (size-avltree2 inst)
      (search-avltree2 inst 0)
      (search-avltree2 inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-avltree2-left 2))
         (root (avltree2::avltree2-root inst)))
    (check-avltree2-error inst)
    (values
      (avltree2::avlnode2-key
        (avltree2::avlnode2-left root))
      (avltree2::avlnode2-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-avltree2-right 2))
         (root (avltree2::avltree2-root inst)))
    (check-avltree2-error inst)
    (values
      (avltree2::avlnode2-key root)
      (avltree2::avlnode2-key
        (avltree2::avlnode2-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-avltree2)))
    (prog1
      (delete-avltree2 inst 10)
      (check-avltree2-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-avltree2)) ret)
    (insert-avltree2 inst 10 20)
    (setq ret (delete-avltree2 inst 10))
    (check-avltree2-error inst)
    (values ret (size-avltree2 inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-avltree2)) ret)
    (insert-avltree2 inst 10 20)
    (setq ret (delete-avltree2 inst 20))
    (check-avltree2-error inst)
    (values ret (size-avltree2 inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-avltree2-left 2)) ret size root key value)
    (setq ret (delete-avltree2 inst 1))
    (check-avltree2-error inst)
    (setq size (size-avltree2 inst))
    (setq root (avltree2::avltree2-root inst))
    (setq key (avltree2::avlnode2-key root))
    (setq value (avltree2::avlnode2-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-avltree2-right 2)) ret size root key value)
    (setq ret (delete-avltree2 inst 0))
    (check-avltree2-error inst)
    (setq size (size-avltree2 inst))
    (setq root (avltree2::avltree2-root inst))
    (setq key (avltree2::avlnode2-key root))
    (setq value (avltree2::avlnode2-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-avltree2-left 2)) ret size root key value)
    (setq ret (delete-avltree2 inst 0))
    (setq size (size-avltree2 inst))
    (setq root (avltree2::avltree2-root inst))
    (setq key (avltree2::avlnode2-key root))
    (setq value (avltree2::avlnode2-value root))
    (check-avltree2-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-avltree2-right 2)) ret size root key value)
    (setq ret (delete-avltree2 inst 1))
    (setq size (size-avltree2 inst))
    (setq root (avltree2::avltree2-root inst))
    (setq key (avltree2::avlnode2-key root))
    (setq value (avltree2::avlnode2-value root))
    (check-avltree2-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-avltree2-left 2)))
    (delete-avltree2 inst 1)
    (check-avltree2-error inst)
    (delete-avltree2 inst 0)
    (check-avltree2-error inst)
    (size-avltree2 inst))
  0)

(deftest delete2.6
  (let ((inst (init-avltree2-left 2)))
    (delete-avltree2 inst 0)
    (check-avltree2-error inst)
    (delete-avltree2 inst 1)
    (check-avltree2-error inst)
    (size-avltree2 inst))
  0)

(deftest delete2.7
  (let ((inst (init-avltree2-right 2)))
    (delete-avltree2 inst 1)
    (check-avltree2-error inst)
    (delete-avltree2 inst 0)
    (check-avltree2-error inst)
    (size-avltree2 inst))
  0)

(deftest delete2.8
  (let ((inst (init-avltree2-right 2)))
    (delete-avltree2 inst 0)
    (check-avltree2-error inst)
    (delete-avltree2 inst 1)
    (check-avltree2-error inst)
    (size-avltree2 inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-avltree2-left 3)))
    (check-avltree2-error inst)
    (delete-avltree2 inst 0)
    (check-avltree2-error inst)
    (size-avltree2 inst))
  2)

(deftest delete3.2
  (let ((inst (init-avltree2-left 3)))
    (check-avltree2-error inst)
    (delete-avltree2 inst 2)
    (check-avltree2-error inst)
    (size-avltree2 inst))
  2)

(deftest delete3.3
  (let ((inst (init-avltree2-left 3)))
    (check-avltree2-error inst)
    (delete-avltree2 inst 1)
    (check-avltree2-error inst)
    (size-avltree2 inst))
  2)

(deftest delete3.4
  (let ((inst (init-avltree2-left 3)))
    (check-avltree2-error inst)
    (delete-avltree2 inst 1)
    (check-avltree2-error inst)
    (size-avltree2 inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-avltree2 (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-avltree2 inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-avltree2-left 4)))
    (check-avltree2-error inst)
    (delete-avltree2 inst 0)
    (check-avltree2-error inst)
    (values
      (size-avltree2 inst)
      (search-avltree2 inst 0)
      (search-check-avltree2 inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-avltree2-left 4)))
    (check-avltree2-error inst)
    (delete-avltree2 inst 1)
    (check-avltree2-error inst)
    (values
      (size-avltree2 inst)
      (search-avltree2 inst 1)
      (search-check-avltree2 inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-avltree2-left 4)))
    (check-avltree2-error inst)
    (delete-avltree2 inst 2)
    (check-avltree2-error inst)
    (values
      (size-avltree2 inst)
      (search-avltree2 inst 2)
      (search-check-avltree2 inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-avltree2-left 4)))
    (check-avltree2-error inst)
    (delete-avltree2 inst 3)
    (check-avltree2-error inst)
    (values
      (size-avltree2 inst)
      (search-avltree2 inst 3)
      (search-check-avltree2 inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-avltree2-left size))
                (right (init-avltree2-right size))
                (insert (init-avltree2-insert size)))))
    (check-avltree2-error inst)
    (delete-avltree2 inst index)
    (check-avltree2-error inst)
    (unless (= (1- size) (size-avltree2 inst))
      (error "size error."))
    (when (search-avltree2 inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-avltree2 inst list)
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
(deftest avltree2-delete.1
  (let ((inst (make-avltree2)))
    (prog1
      (delete-avltree2 inst 10)
      (check-avltree2-error inst)))
  nil)

(deftest avltree2-delete.2
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 20)
    (prog1
      (delete-avltree2 inst 10)
      (check-avltree2-error inst)))
  t)

(deftest avltree2-delete.3
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 20)
    (delete-avltree2 inst 10)
    (check-avltree2-error inst)
    (values
      (empty-avltree2 inst)
      (size-avltree2 inst)))
  t 0)

(deftest avltree2-delete.4
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 20)
    (delete-avltree2 inst 30)
    (check-avltree2-error inst)
    (values
      (empty-avltree2 inst)
      (size-avltree2 inst)))
  nil 1)

(deftest avltree2-delete.5
  (let ((inst (make-avltree2)))
    (dotimes (i 20)
      (insert-avltree2 inst i (* 100 i)))
    (delete-avltree2 inst 5)
    (delete-avltree2 inst 6)
    (delete-avltree2 inst 7)
    (check-avltree2-error inst)
    (values
      (search-avltree2 inst 4)
      (search-avltree2 inst 6)
      (search-avltree2 inst 100)
      (size-avltree2 inst)))
  400 nil nil 17)

(deftest avltree2-delete.6
  (let ((inst (make-avltree2)))
    (dotimes (i 20)
      (insert-avltree2 inst i (* 100 i)))
    (dotimes (i 20)
      (delete-avltree2 inst i))
    (values
      (empty-avltree2 inst)
      (size-avltree2 inst)))
  t 0)

(deftest avltree2-min.1
  (let ((inst (make-avltree2)))
    (insert-avltree2 inst 10 100)
    (insert-avltree2 inst 20 200)
    (insert-avltree2 inst 30 300)
    (min-avltree2 inst))
  10 t)


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
  (let ((inst (make-avltree2))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-avltree2 inst vector)
    (check-avltree2-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-avltree2)))
    (dotimes (i +size+)
      (insert-avltree2 inst i (* 100 i)))
    (check-avltree2-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

