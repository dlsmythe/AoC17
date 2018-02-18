;;; Run with: sbcl --noinform --load adv17-10.lisp <hashstr>

(ql:quickload "split-sequence" :silent t)
(load "knot.lisp")

(defparameter *part* 2)
(defparameter *default-input* "227,169,3,166,246,201,0,47,1,255,2,254,96,3,97,144")

(defun main (args)
  ;;(declare (ignore args))
  (let ((str (if (> (length args) 1)
		 (car (last args))
		 *default-input*)))
    (if (= 2 *part*)
	(format t "~A~%" (dls:knot-hash str))
	(let ((lens (apply #'vector (loop for s in (split-sequence:split-sequence #\, *default-input*)
				       collect (parse-integer s))))
	      (inp (apply #'vector (loop for i below 256 collect i))))
	  (dls:knot-round lens inp)
	  (format t "~A * ~A = ~A~%" (elt inp 0) (elt inp 1) (* (elt inp 0) (elt inp 1))))))
  0)
 
(sb-ext:exit :code (main sb-ext:*POSIX-ARGV*))


