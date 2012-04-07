(defpackage :php-lookup (:use :common-lisp)
            (:export :populate-table :symbol-lookup :constant-lookup))
(in-package :php-lookup)

(defparameter *php-root* "http://www.php.net/manual/en/")
(defparameter *php-suffix* ".php")

(defparameter *php-file* (merge-pathnames "php-symbols.lisp-expr"
                                          #.*compile-file-pathname*))
(defparameter *php-constants-file* (merge-pathnames "php-constants.lisp-expr"
                                          #.*compile-file-pathname*))
(defvar *table* nil)
(defvar *constants* '())

(defun populate-table ()
  (with-open-file (r *php-file* :direction :input)
    (setf *table* (make-hash-table :test #'equalp))
    (let ((s (read r)))
      (loop for i in s do (setf (gethash (car i) *table*) (cdr i))))
    'done)
  (setf *populated-p* t))

(defun symbol-lookup (symbol)
  (unless *table*
    (populate-table))
  (multiple-value-bind (val found)
      (gethash symbol *table*)
    (if found
        (concatenate 'string *php-root*
                     val
                     *php-suffix*))))

(defun populate-constants ()
  (with-open-file (r *php-constants-file* :direction :input)
    (when r
      (loop for l = (read-line r nil)
         while l do (pushnew l *constants*)))))

(defun constant-lookup (constant)
  (unless *constants*
    (populate-constants))
  (find constant *constants* :test #'string=))
