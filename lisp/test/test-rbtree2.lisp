(load #p"rbtree2.lisp")
(load #p"test/rbtree2-check.lisp")

#+sbcl (require 'sb-rt)
(defpackage work (:use common-lisp rbtree2 #+npt npt-rt #+sbcl sb-rt))
(in-package work)

(deftest rbtree2.1
  (let ((inst (make-rbtree2)))
    (check-rbtree2-error inst)
    (values
      (empty-rbtree2 inst)
      (size-rbtree2 inst)))
  t 0)

(deftest rbtree2.2
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 20)
    (check-rbtree2-error inst)
    (search-rbtree2 inst 10))
  20 t)

(deftest rbtree2.3
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 20)
    (check-rbtree2-error inst)
    (search-rbtree2 inst 20))
  nil nil)

(deftest rbtree2.4
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 20)
    (check-rbtree2-error inst)
    (values
      (empty-rbtree2 inst)
      (size-rbtree2 inst)))
  nil 1)

(deftest rbtree2.5
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 :aaa)
    (insert-rbtree2 inst 20 :bbb)
    (check-rbtree2-error inst)
    (values
      (empty-rbtree2 inst)
      (size-rbtree2 inst)))
  nil 2)

(deftest rbtree2.6
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 :aaa)
    (insert-rbtree2 inst 20 :bbb)
    (check-rbtree2-error inst)
    (values
      (search-rbtree2 inst 10)
      (search-rbtree2 inst 20)
      (search-rbtree2 inst 30)))
  :aaa :bbb nil)

(deftest rbtree2.7
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 20 :bbb)
    (insert-rbtree2 inst 10 :aaa)
    (check-rbtree2-error inst)
    (values
      (search-rbtree2 inst 10)
      (search-rbtree2 inst 20)
      (search-rbtree2 inst 30)))
  :aaa :bbb nil)

(deftest rbtree2.8
  (let ((inst (make-rbtree2)))
    (dotimes (i 20)
      (insert-rbtree2 inst i (* 100 i))
      (check-rbtree2-error inst))
    (values
      (search-rbtree2 inst 5)
      (search-rbtree2 inst 6)
      (search-rbtree2 inst 100)
      (size-rbtree2 inst)))
  500 600 nil 20)

(deftest rbtree2.9
  (let ((inst (make-rbtree2)))
    (dotimes (i 20)
      (let ((i (- 20 i)))
        (insert-rbtree2 inst i (* 100 i))))
    (check-rbtree2-error inst)
    (values
      (search-rbtree2 inst 5)
      (search-rbtree2 inst 6)
      (search-rbtree2 inst 100)
      (size-rbtree2 inst)))
  500 600 nil 20)


;;
;;  init
;;
(deftest init.1
  (let ((inst (init-rbtree2 (constantly 10) 0)))
    (check-rbtree2-error inst)
    (rbtree2-p inst))
  t)

(defun init-rbtree2-left (size)
  (let ((index 0))
    (init-rbtree2
      (lambda ()
        (multiple-value-prog1
          (values index (* index 10))
          (incf index 1)))
      size)))

(defun init-rbtree2-right (size)
  (let ((index size))
    (init-rbtree2
      (lambda ()
        (decf index 1)
        (values index (* index 10)))
      size nil t)))

(defun init-rbtree2-insert (size)
  (let ((inst (make-rbtree2)))
    (dotimes (index size inst)
      (insert-rbtree2 inst index (* index 10)))))

(deftest init.2
  (let ((inst (init-rbtree2-left 1)))
    (check-rbtree2-error inst)
    (rbtree2-p inst))
  t)

(deftest init.3
  (let ((inst (init-rbtree2-right 1)))
    (check-rbtree2-error inst)
    (rbtree2-p inst))
  t)

(deftest init.4
  (let ((inst (init-rbtree2-left 1)))
    (check-rbtree2-error inst)
    (values
      (size-rbtree2 inst)
      (search-rbtree2 inst 0)))
  1 0)

(deftest init.5
  (let ((inst (init-rbtree2-right 1)))
    (check-rbtree2-error inst)
    (values
      (size-rbtree2 inst)
      (search-rbtree2 inst 0)))
  1 0)

(deftest init.6
  (let ((inst (init-rbtree2-left 2)))
    (check-rbtree2-error inst)
    (values
      (size-rbtree2 inst)
      (search-rbtree2 inst 0)
      (search-rbtree2 inst 1)))
  2 0 10)

(deftest init.7
  (let ((inst (init-rbtree2-right 2)))
    (check-rbtree2-error inst)
    (values
      (size-rbtree2 inst)
      (search-rbtree2 inst 0)
      (search-rbtree2 inst 1)))
  2 0 10)

(deftest init.8
  (let* ((inst (init-rbtree2-left 2))
         (root (rbtree2::rbtree2-root inst)))
    (check-rbtree2-error inst)
    (values
      (rbtree2::rbnode2-key
        (rbtree2::rbnode2-left root))
      (rbtree2::rbnode2-key root)))
  0 1)

(deftest init.9
  (let* ((inst (init-rbtree2-right 2))
         (root (rbtree2::rbtree2-root inst)))
    (check-rbtree2-error inst)
    (values
      (rbtree2::rbnode2-key root)
      (rbtree2::rbnode2-key
        (rbtree2::rbnode2-right root))))
  0 1)


;;
;;  delete
;;

;;  delete1
(deftest delete0.1
  (let ((inst (make-rbtree2)))
    (prog1
      (delete-rbtree2 inst 10)
      (check-rbtree2-error inst)))
  nil)

(deftest delete1.1
  (let ((inst (make-rbtree2)) ret)
    (insert-rbtree2 inst 10 20)
    (setq ret (delete-rbtree2 inst 10))
    (check-rbtree2-error inst)
    (values ret (size-rbtree2 inst)))
  t 0)

(deftest delete1.2
  (let ((inst (make-rbtree2)) ret)
    (insert-rbtree2 inst 10 20)
    (setq ret (delete-rbtree2 inst 20))
    (check-rbtree2-error inst)
    (values ret (size-rbtree2 inst)))
  nil 1)


;;  delete2
(deftest delete2.1
  (let ((inst (init-rbtree2-left 2)) ret size root key value)
    (setq ret (delete-rbtree2 inst 1))
    (check-rbtree2-error inst)
    (setq size (size-rbtree2 inst))
    (setq root (rbtree2::rbtree2-root inst))
    (setq key (rbtree2::rbnode2-key root))
    (setq value (rbtree2::rbnode2-value root))
    (values ret size key value))
  t 1 0 0)

(deftest delete2.2
  (let ((inst (init-rbtree2-right 2)) ret size root key value)
    (setq ret (delete-rbtree2 inst 0))
    (check-rbtree2-error inst)
    (setq size (size-rbtree2 inst))
    (setq root (rbtree2::rbtree2-root inst))
    (setq key (rbtree2::rbnode2-key root))
    (setq value (rbtree2::rbnode2-value root))
    (values ret size key value))
  t 1 1 10)

(deftest delete2.3
  (let ((inst (init-rbtree2-left 2)) ret size root key value)
    (setq ret (delete-rbtree2 inst 0))
    (setq size (size-rbtree2 inst))
    (setq root (rbtree2::rbtree2-root inst))
    (setq key (rbtree2::rbnode2-key root))
    (setq value (rbtree2::rbnode2-value root))
    (check-rbtree2-error inst)
    (values ret size key value))
  t 1 1 10)

(deftest delete2.4
  (let ((inst (init-rbtree2-right 2)) ret size root key value)
    (setq ret (delete-rbtree2 inst 1))
    (setq size (size-rbtree2 inst))
    (setq root (rbtree2::rbtree2-root inst))
    (setq key (rbtree2::rbnode2-key root))
    (setq value (rbtree2::rbnode2-value root))
    (check-rbtree2-error inst)
    (values ret size key value))
  t 1 0 0)

(deftest delete2.5
  (let ((inst (init-rbtree2-left 2)))
    (delete-rbtree2 inst 1)
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 0)
    (check-rbtree2-error inst)
    (size-rbtree2 inst))
  0)

(deftest delete2.6
  (let ((inst (init-rbtree2-left 2)))
    (delete-rbtree2 inst 0)
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 1)
    (check-rbtree2-error inst)
    (size-rbtree2 inst))
  0)

(deftest delete2.7
  (let ((inst (init-rbtree2-right 2)))
    (delete-rbtree2 inst 1)
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 0)
    (check-rbtree2-error inst)
    (size-rbtree2 inst))
  0)

(deftest delete2.8
  (let ((inst (init-rbtree2-right 2)))
    (delete-rbtree2 inst 0)
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 1)
    (check-rbtree2-error inst)
    (size-rbtree2 inst))
  0)


;;  delete3
(deftest delete3.1
  (let ((inst (init-rbtree2-left 3)))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 0)
    (check-rbtree2-error inst)
    (size-rbtree2 inst))
  2)

(deftest delete3.2
  (let ((inst (init-rbtree2-left 3)))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 2)
    (check-rbtree2-error inst)
    (size-rbtree2 inst))
  2)

(deftest delete3.3
  (let ((inst (init-rbtree2-left 3)))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 1)
    (check-rbtree2-error inst)
    (size-rbtree2 inst))
  2)

(deftest delete3.4
  (let ((inst (init-rbtree2-left 3)))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 1)
    (check-rbtree2-error inst)
    (size-rbtree2 inst))
  2)


;;  delete4
;;    2
;;   1 3
;;  0

(defun search-check-rbtree2 (inst list)
  (dolist (x list t)
    (multiple-value-bind (ignore check) (search-rbtree2 inst x)
      (declare (ignore ignore))
      (unless check
        (return nil)))))

(deftest delete4.1
  (let ((inst (init-rbtree2-left 4)))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 0)
    (check-rbtree2-error inst)
    (values
      (size-rbtree2 inst)
      (search-rbtree2 inst 0)
      (search-check-rbtree2 inst '(1 2 3))))
  3 nil t)

(deftest delete4.2
  (let ((inst (init-rbtree2-left 4)))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 1)
    (check-rbtree2-error inst)
    (values
      (size-rbtree2 inst)
      (search-rbtree2 inst 1)
      (search-check-rbtree2 inst '(0 2 3))))
  3 nil t)

(deftest delete4.3
  (let ((inst (init-rbtree2-left 4)))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 2)
    (check-rbtree2-error inst)
    (values
      (size-rbtree2 inst)
      (search-rbtree2 inst 2)
      (search-check-rbtree2 inst '(0 1 3))))
  3 nil t)

(deftest delete4.4
  (let ((inst (init-rbtree2-left 4)))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst 3)
    (check-rbtree2-error inst)
    (values
      (size-rbtree2 inst)
      (search-rbtree2 inst 3)
      (search-check-rbtree2 inst '(0 1 2))))
  3 nil t)


;;  delete all
(defun delete-all-index (direct size index)
  (let ((inst (ecase direct
                (left (init-rbtree2-left size))
                (right (init-rbtree2-right size))
                (insert (init-rbtree2-insert size)))))
    (check-rbtree2-error inst)
    (delete-rbtree2 inst index)
    (check-rbtree2-error inst)
    (unless (= (1- size) (size-rbtree2 inst))
      (error "size error."))
    (when (search-rbtree2 inst index)
      (error "search error."))
    (let* ((list (loop for i from 0 below size collect i))
           (list (delete index list)))
      (unless (search-check-rbtree2 inst list)
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
(deftest rbtree2-delete.1
  (let ((inst (make-rbtree2)))
    (prog1
      (delete-rbtree2 inst 10)
      (check-rbtree2-error inst)))
  nil)

(deftest rbtree2-delete.2
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 20)
    (prog1
      (delete-rbtree2 inst 10)
      (check-rbtree2-error inst)))
  t)

(deftest rbtree2-delete.3
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 20)
    (delete-rbtree2 inst 10)
    (check-rbtree2-error inst)
    (values
      (empty-rbtree2 inst)
      (size-rbtree2 inst)))
  t 0)

(deftest rbtree2-delete.4
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 20)
    (delete-rbtree2 inst 30)
    (check-rbtree2-error inst)
    (values
      (empty-rbtree2 inst)
      (size-rbtree2 inst)))
  nil 1)

(deftest rbtree2-delete.5
  (let ((inst (make-rbtree2)))
    (dotimes (i 20)
      (insert-rbtree2 inst i (* 100 i)))
    (delete-rbtree2 inst 5)
    (delete-rbtree2 inst 6)
    (delete-rbtree2 inst 7)
    (check-rbtree2-error inst)
    (values
      (search-rbtree2 inst 4)
      (search-rbtree2 inst 6)
      (search-rbtree2 inst 100)
      (size-rbtree2 inst)))
  400 nil nil 17)

(deftest rbtree2-delete.6
  (let ((inst (make-rbtree2)))
    (dotimes (i 20)
      (insert-rbtree2 inst i (* 100 i)))
    (dotimes (i 20)
      (delete-rbtree2 inst i))
    (values
      (empty-rbtree2 inst)
      (size-rbtree2 inst)))
  t 0)

(deftest rbtree2-min.1
  (let ((inst (make-rbtree2)))
    (insert-rbtree2 inst 10 100)
    (insert-rbtree2 inst 20 200)
    (insert-rbtree2 inst 30 300)
    (min-rbtree2 inst))
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
  (let ((inst (make-rbtree2))
        (vector (make-array +size+ :initial-element nil)))
    (dotimes (i +size+)
      (setf (aref vector i) (cons i (* 100 i))))
    (init-associate-rbtree2 inst vector)
    (check-rbtree2-error inst)
    (format t "~S~%" inst)))

(defun main-insert ()
  (let ((inst (make-rbtree2)))
    (dotimes (i +size+)
      (insert-rbtree2 inst i (* 100 i)))
    (check-rbtree2-error inst)
    (format t "~S~%" inst)))

;(main-init)
;(main-insert)

