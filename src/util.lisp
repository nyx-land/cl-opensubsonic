(in-package :sonic)

(defun split-api (name)
  (subseq name (1+ (position #\/ name))))

(defun remove-kebab (sym)
  (let ((split (uiop:split-string
                (string-downcase
                 (if (position #\/ (symbol-name sym))
                     (split-api (symbol-name sym))
                     (symbol-name sym)))
                :separator '(#\-))))
    (format nil "~a~{~:(~a~)~}"
            (car split) (cdr split))))
