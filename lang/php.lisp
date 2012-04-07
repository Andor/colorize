;; lang/php.lisp

(in-package :colorize)

(defvar *php-open-parens* "([{")
(defvar *php-close-parens* ")]}")
(defvar *php-open-tags* '("<?php" "<?"))
(defvar *php-close-tags* '("?>"))

(defvar *php-reserved-words*
  '("abstract" "and" "array" "as" "break" "case" "catch" "cfunction" "class"
    "clone" "const" "continue" "declare" "default" "do" "else" "elseif"
    "enddeclare" "endfor" "endforeach" "endif" "endswitch" "endwhile" "extends"
    "final" "for" "foreach" "function" "global" "goto" "if" "implements"
    "interface" "instanceof" "namespace" "new" "old_function" "or" "private"
    "protected" "public" "static" "switch" "throw" "try" "use" "var" "while" "xor"
    "die" "echo" "empty" "exit" "eval" "include" "include_once" "isset" "list"
    "require" "require_once" "return" "print" "unset" "__halt_compiler"))

(defparameter *php-begin-word* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
(defparameter *php-terminators* '(#\space #\return #\tab #\newline #\. #\' #\" #\# #\, #\& #\= #\( #\) #\[ #\] #\{ #\} #\< #\> #\; #\- #\+ #\* #\/ #\\))

(define-coloring-type :php "PHP"
  :default-mode :normal
  :transitions
  ((:normal
    ((scan-any *php-open-tags*)
      (set-mode :php
                :until (scan-any *php-close-tags*))))
   (:php
    ((or
      (scan-any *php-open-parens*)
      (scan-any *php-close-parens*))
     (set-mode :paren-ish
               :until (advance 1)
               :advancing nil))
    #+nil
    ((scan "case ")
     (set-mode :paren-ish
               :until (scan "break;")))
    ((scan "/*") ; multiline comments
     (set-mode :comment
               :until (scan "*/")))
    ((scan-any '(#\/ #\#)) ; 1-line comments
     (set-mode :comment
               :until (scan-any '(#\return #\newline))))
    ((scan #\$) ; variables
     (set-mode :variable
	       :until (scan-any *php-terminators*)
	       :advancing nil))
    ((scan-any *php-begin-word*)
     (set-mode :word-ish
               :until (scan-any *php-terminators*)
               :advancing nil))
    ((scan #\")
     (set-mode :string-2
               :until (scan #\")))
    ((scan #\')
     (set-mode :string
               :until (scan #\'))))
   (:string
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1))))
   (:string-2
    ((scan #\\)
     (set-mode :single-escape
               :until (advance 1)))
    ((scan #\$) ; variables in strings
     (set-mode :variable
	       :until (scan-any *php-terminators*)
	       :advancing nil))))
  :formatter-variables
  ((paren-counter 0))
  :formatter-after-hook (lambda nil
                          (format nil "~{~A~}"
                                  (loop for i from paren-counter downto 1
                                     collect "</span></span>")))
  :formatters
  ((:php
    (lambda (type s)
      (declare (ignore type))
              s))
   (:normal
    (lambda (type s)
      (declare (ignore type))
      s))
   (:matching
    (lambda (type s)
      (declare (ignore type))
      s))
   (:comment
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"comment\">~A</span>"
              s)))
   (:string
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   (:string-2
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"string\">~A</span>"
              s)))
   (:variable 
    (lambda (type s)
      (declare (ignore type))
      (format nil "<span class=\"variable\">~A</span>"
	      s)))
   (:single-escape
    (lambda (type s)
      (call-formatter (cdr type) s)))
   (:paren-ish
    (lambda (type s)
      (declare (ignore type))
      (let ((open nil)
            (count 0))
        (if (eql (length s) 1)
            (progn
              (when (member (elt s 0) (coerce *php-open-parens* 'list))
                (setf open t)
                (setf count (mod paren-counter 6))
                (incf paren-counter))
              (when (member (elt s 0) (coerce *php-close-parens* 'list))
                (setf open nil)
                (decf paren-counter)
                (setf count (mod paren-counter 6)))
              (if open
                  (format nil "<span class=\"paren~A\">~A<span class=\"~A\">"
                          (1+ count) s *css-background-class*)
                  (format nil "</span>~A</span>"
                          s)))
            s))))
   (:word-ish
    (lambda (type s)
      (declare (ignore type))
      (let ((result s)
            (url (if (find-package :php-lookup)
                     (funcall (symbol-function (intern "SYMBOL-LOOKUP" :php-lookup))
                              s)))
            (class
             (if (find s *php-reserved-words* :test #'string=)
                 "symbol"
                 (if (find-package :php-lookup)
                     (if (funcall (symbol-function (intern "CONSTANT-LOOKUP" :php-lookup))
                                  s)
                         "special")))))
        
        (if class
            (setf result (format nil "<span class=\"~A\">~A</span>" class s)))
        (if url
            (format nil "<a href=\"~A\" class=\"symbol\">~A</a>" url result)
            result)))))))
