(defpackage :php-lookup (:use :common-lisp)
            (:export :populate-table :symbol-lookup))
(in-package :php-lookup)

(defparameter *php-root* "http://www.php.net/manual/en/")
(defparameter *php-suffix* ".php")

(defparameter *php-file* (merge-pathnames "php-symbols.lisp-expr"
                                          #.*compile-file-pathname*))
(defvar *table* nil)

(defvar *populated-p* nil)

(defun populate-table ()
  (unless *populated-p*
    (with-open-file (r *php-file* :direction :input)
      (setf *table* (make-hash-table :test #'equalp))
      (let ((s (read r)))
        (loop for i in s do (setf (gethash (car i) *table*) (cdr i))))
      'done)
    (setf *populated-p* t)))

(defun symbol-lookup (symbol)
  (unless *populated-p*
    (populate-table))
  (multiple-value-bind (val found)
      (gethash symbol *table*)
    (if found
        (concatenate 'string *php-root*
                     val
                     *php-suffix*))))
