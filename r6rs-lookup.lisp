(defpackage :r6rs-lookup (:use :common-lisp)
            (:export :populate-table :symbol-lookup))
(in-package :r6rs-lookup)

(defparameter *r6rs-root* "http://www.r6rs.org/final/html/")

(defparameter *r6rs-libs-file*
  (merge-pathnames "r6rs-symbols-libs.lisp-expr"
		   #.*compile-file-pathname*))
(defparameter *r6rs-keywords-file*
  (merge-pathnames "r6rs-symbols-keywords.lisp-expr"
		   #.*compile-file-pathname*))

(defvar *table* nil)

(defvar *populated-p* nil)

(defun populate-table ()
  (unless *populated-p*
    (setf *table* (make-hash-table :test #'equalp))
    (with-open-file (r *r6rs-libs-file* :direction :input)
      (let ((s (read r)))
        (loop for i in s do (setf (gethash (car i) *table*) (concatenate 'string "r6rs-lib/" (cdr i)))))
      'done)
    (with-open-file (r *r6rs-keywords-file* :direction :input)
      (let ((s (read r)))
        (loop for i in s do (setf (gethash (car i) *table*) (concatenate 'string "r6rs/" (cdr i)))))
      'done)
    (setf *populated-p* t)))

(defun symbol-lookup (symbol)
  (unless *populated-p*
    (populate-table))
  (multiple-value-bind (val found)
      (gethash symbol *table*)
    (if found
        (concatenate 'string *r6rs-root*
                     val))))
