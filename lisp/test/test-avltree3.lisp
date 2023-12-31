(load #p"avltree3.lisp")
(load #p"test/avltree3-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp avltree3 #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest avltree3.1
  (let ((inst (make-avltree3)))
    (check-avltree3-error inst)
    (values
      (empty-avltree3 inst)
      (size-avltree3 inst)))
  t 0)

(deftest avltree3.2
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 20)
    (check-avltree3-error inst)
    (search-avltree3 inst 10))
  20 t)

(deftest avltree3.3
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 20)
    (check-avltree3-error inst)
    (search-avltree3 inst 20))
  nil nil)

(deftest avltree3.4
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 20)
    (check-avltree3-error inst)
    (values
      (empty-avltree3 inst)
      (size-avltree3 inst)))
  nil 1)

(deftest avltree3.5
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 :aaa)
    (insert-avltree3 inst 20 :bbb)
    (check-avltree3-error inst)
    (values
      (empty-avltree3 inst)
      (size-avltree3 inst)))
  nil 2)

(deftest avltree3.6
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 :aaa)
    (insert-avltree3 inst 20 :bbb)
    (check-avltree3-error inst)
    (values
      (search-avltree3 inst 10)
      (search-avltree3 inst 20)
      (search-avltree3 inst 30)))
  :aaa :bbb nil)

(deftest avltree3.7
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 20 :bbb)
    (insert-avltree3 inst 10 :aaa)
    (check-avltree3-error inst)
    (values
      (search-avltree3 inst 10)
      (search-avltree3 inst 20)
      (search-avltree3 inst 30)))
  :aaa :bbb nil)

(deftest avltree3.8
  (let ((inst (make-avltree3)))
    (dotimes (i 20)
      (insert-avltree3 inst i (* 100 i))
      (check-avltree3-error inst))
    (values
      (search-avltree3 inst 5)
      (search-avltree3 inst 6)
      (search-avltree3 inst 100)
      (size-avltree3 inst)))
  500 600 nil 20)

(deftest avltree3.9
  (let ((inst (make-avltree3)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-avltree3 inst i (* 100 i))))
    (check-avltree3-error inst)
    (values
      (search-avltree3 inst 5)
      (search-avltree3 inst 6)
      (search-avltree3 inst 100)
      (size-avltree3 inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-avltree3 (constantly 10) 0)))
    (check-avltree3-error inst)
    (avltree3-p inst))
  t)

(defun init-avltree3-left (size)
  (let ((index 0))
    (init-avltree3
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-avltree3-right (size)
  (let ((index size))
    (init-avltree3
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-avltree3-insert (size)
  (let ((inst (make-avltree3)))
    (dotimes (index size inst)
      (insert-avltree3 inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-avltree3-left 1)))
    (check-avltree3-error inst)
    (avltree3-p inst))
  t)

(deftest init.3
  (let ((inst (init-avltree3-right 1)))
    (check-avltree3-error inst)
    (avltree3-p inst))
  t)

(deftest init.4
  (let ((inst (init-avltree3-left 1)))
    (check-avltree3-error inst)
    (values
      (size-avltree3 inst)
      (search-avltree3 inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-avltree3-right 1)))
    (check-avltree3-error inst)
    (values
      (size-avltree3 inst)
      (search-avltree3 inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-avltree3-left 2)))
    (check-avltree3-error inst)
    (values
      (size-avltree3 inst)
      (search-avltree3 inst 0)
      (search-avltree3 inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-avltree3-right 2)))
    (check-avltree3-error inst)
    (values
      (size-avltree3 inst)
      (search-avltree3 inst 0)
      (search-avltree3 inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-avltree3-left 2))
         (root (avltree3::avltree3-root inst)))
    (check-avltree3-error inst)
    (values
      (avltree3::avlnode3-key
        (avltree3::avlnode3-left root))
      (avltree3::avlnode3-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-avltree3-right 2))
         (root (avltree3::avltree3-root inst)))
    (check-avltree3-error inst)
    (values
      (avltree3::avlnode3-key root)
      (avltree3::avlnode3-key
        (avltree3::avlnode3-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-avltree3)))
    (prog1
      (delete-avltree3 inst 10)
      (check-avltree3-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-avltree3)) ret)
    (insert-avltree3 inst 10 20)
    (setq ret (delete-avltree3 inst 10))
    (check-avltree3-error inst)
    (values ret (size-avltree3 inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-avltree3)) ret)
    (insert-avltree3 inst 10 20)
    (setq ret (delete-avltree3 inst 20))
    (check-avltree3-error inst)
    (values ret (size-avltree3 inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-avltree3-left 2)) ret size root key value)
    (setq ret (delete-avltree3 inst 1))
    (check-avltree3-error inst)
    (setq size (size-avltree3 inst))
    (setq root (avltree3::avltree3-root inst))
    (setq key (avltree3::avlnode3-key root))
    (setq value (avltree3::avlnode3-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-avltree3-right 2)) ret size root key value)
    (setq ret (delete-avltree3 inst 0))
    (check-avltree3-error inst)
    (setq size (size-avltree3 inst))
    (setq root (avltree3::avltree3-root inst))
    (setq key (avltree3::avlnode3-key root))
    (setq value (avltree3::avlnode3-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-avltree3-left 2)) ret size root key value)
    (setq ret (delete-avltree3 inst 0))
    (setq size (size-avltree3 inst))
    (setq root (avltree3::avltree3-root inst))
    (setq key (avltree3::avlnode3-key root))
    (setq value (avltree3::avlnode3-value root))
    (check-avltree3-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-avltree3-right 2)) ret size root key value)
    (setq ret (delete-avltree3 inst 1))
    (setq size (size-avltree3 inst))
    (setq root (avltree3::avltree3-root inst))
    (setq key (avltree3::avlnode3-key root))
    (setq value (avltree3::avlnode3-value root))
    (check-avltree3-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-avltree3-left 2)))
    (delete-avltree3 inst 1)
    (check-avltree3-error inst)
    (delete-avltree3 inst 0)
    (check-avltree3-error inst)
    (size-avltree3 inst))
  0)

(deftest delete2.6
  (let ((inst (init-avltree3-left 2)))
    (delete-avltree3 inst 0)
    (check-avltree3-error inst)
    (delete-avltree3 inst 1)
    (check-avltree3-error inst)
    (size-avltree3 inst))
  0)

(deftest delete2.7
  (let ((inst (init-avltree3-right 2)))
    (delete-avltree3 inst 1)
    (check-avltree3-error inst)
    (delete-avltree3 inst 0)
    (check-avltree3-error inst)
    (size-avltree3 inst))
  0)

(deftest delete2.8
  (let ((inst (init-avltree3-right 2)))
    (delete-avltree3 inst 0)
    (check-avltree3-error inst)
    (delete-avltree3 inst 1)
    (check-avltree3-error inst)
    (size-avltree3 inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-avltree3-left 3)))
    (check-avltree3-error inst)
    (delete-avltree3 inst 0)
    (check-avltree3-error inst)
    (size-avltree3 inst))
  2)

(deftest delete3.2
  (let ((inst (init-avltree3-left 3)))
    (check-avltree3-error inst)
    (delete-avltree3 inst 2)
    (check-avltree3-error inst)
    (size-avltree3 inst))
  2)

(deftest delete3.3
  (let ((inst (init-avltree3-left 3)))
    (check-avltree3-error inst)
    (delete-avltree3 inst 1)
    (check-avltree3-error inst)
    (size-avltree3 inst))
  2)

(deftest delete3.4
  (let ((inst (init-avltree3-left 3)))
    (check-avltree3-error inst)
    (delete-avltree3 inst 1)
    (check-avltree3-error inst)
    (size-avltree3 inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-avltree3 (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-avltree3 inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-avltree3-left 4)))
    (check-avltree3-error inst)
    (delete-avltree3 inst 0)
    (check-avltree3-error inst)
    (values
      (size-avltree3 inst)
      (search-avltree3 inst 0)
      (search-check-avltree3 inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-avltree3-left 4)))
    (check-avltree3-error inst)
    (delete-avltree3 inst 1)
    (check-avltree3-error inst)
    (values
      (size-avltree3 inst)
      (search-avltree3 inst 1)
      (search-check-avltree3 inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-avltree3-left 4)))
    (check-avltree3-error inst)
    (delete-avltree3 inst 2)
    (check-avltree3-error inst)
    (values
      (size-avltree3 inst)
      (search-avltree3 inst 2)
      (search-check-avltree3 inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-avltree3-left 4)))
    (check-avltree3-error inst)
    (delete-avltree3 inst 3)
    (check-avltree3-error inst)
    (values
      (size-avltree3 inst)
      (search-avltree3 inst 3)
      (search-check-avltree3 inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-avltree3-left size))
                (right (init-avltree3-right size))
                (insert (init-avltree3-insert size)))))
    (check-avltree3-error inst)
    (delete-avltree3 inst index)
    (check-avltree3-error inst)
    (unless (= (1- size) (size-avltree3 inst))
      (error "size error."))
    (when (search-avltree3 inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-avltree3 inst list)
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
(deftest avltree3-delete.1
  (let ((inst (make-avltree3)))
    (prog1
      (delete-avltree3 inst 10)
      (check-avltree3-error inst)))
  nil)

(deftest avltree3-delete.2
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 20)
    (prog1
      (delete-avltree3 inst 10)
      (check-avltree3-error inst)))
  t)

(deftest avltree3-delete.3
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 20)
    (delete-avltree3 inst 10)
    (check-avltree3-error inst)
    (values
      (empty-avltree3 inst)
      (size-avltree3 inst)))
  t 0)

(deftest avltree3-delete.4
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 20)
    (delete-avltree3 inst 30)
    (check-avltree3-error inst)
    (values
      (empty-avltree3 inst)
      (size-avltree3 inst)))
  nil 1)

(deftest avltree3-delete.5
  (let ((inst (make-avltree3)))
    (dotimes (i 20)
      (insert-avltree3 inst i (* 100 i)))
    (delete-avltree3 inst 5)
    (delete-avltree3 inst 6)
    (delete-avltree3 inst 7)
    (check-avltree3-error inst)
    (values
      (search-avltree3 inst 4)
      (search-avltree3 inst 6)
      (search-avltree3 inst 100)
      (size-avltree3 inst)))
  400 nil nil 17)

(deftest avltree3-delete.6
  (let ((inst (make-avltree3)))
    (dotimes (i 20)
      (insert-avltree3 inst i (* 100 i)))
    (dotimes (i 20)
      (delete-avltree3 inst i))
    (values
      (empty-avltree3 inst)
      (size-avltree3 inst)))
  t 0)

(deftest avltree3-min.1
  (let ((inst (make-avltree3)))
    (insert-avltree3 inst 10 100)
    (insert-avltree3 inst 20 200)
    (insert-avltree3 inst 30 300)
    (min-avltree3 inst))
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
  (let ((inst (make-avltree3))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-avltree3 inst vector)
    (check-avltree3-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-avltree3)))
    (dotimes (i +size+)
      (insert-avltree3 inst i (* 100 i)))
    (check-avltree3-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

