(defpackage :dls
  (:use :common-lisp)
  (:export :read-file-into-string))

(in-package :dls)

(defun read-file-into-string (fname)
  (with-open-file (in fname)
    (let ((data (make-string (file-length in))))
      (read-sequence data in)
      ;;(format t "len ~A data is ~A~%" (file-length in) data)
      (values data (file-length in)))))
