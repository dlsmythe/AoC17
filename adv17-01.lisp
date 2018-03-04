;;; In this one, I was getting reacquainted.  New things:
;;; - read a file into a buffer
;;; - return multiple values and bind them
;;; - trivial macro
;;; - convert char <=> ASCII-code

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(defun read-file-into-string (fname)
  (with-open-file (in fname)
    (let ((data (make-string (file-length in))))
      (read-sequence data in)
      ;;(format t "len ~A data is ~A~%" (file-length in) data)
      (values data (file-length in)))))

(defmacro OTHER (i maxlen) `(mod (+ ,i (if (= 1 part) 1 (/ ,maxlen 2))) ,maxlen))

(defun do-part (part)
  (multiple-value-bind (inbuf-plus-newline inbuflen) (read-file-into-string "adv17-01.input")
    (let ((inbuf (remove #\n inbuf-plus-newline))
	  (sum 0)
	  (zero-code (char-code #\0)))
      (decf inbuflen)
      (dotimes (i inbuflen)
	(let* ((c (char inbuf i))
	       (cc (char-code c)))
	  (when (char= c (char inbuf (OTHER i inbuflen)))
	    ;; (format t "~a += ~a~%" sum (- cc zero-code))
	    (incf sum (- cc zero-code)))))
		
      (format t "part ~a: ~a~%" part sum))))

(do-part 1)
(do-part 2)

(sb-ext:exit :code 0)
