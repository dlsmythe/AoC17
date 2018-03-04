;;; sbcl --noinform --load adv17-15.lisp
;;;  -p n    part 1 or 2
;;;  -e      use example values
;;;
;;; New here:
;;; - just the macro.

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)

(defparameter *part* 1)
(defparameter *example* nil)

(defmacro gen (name ex-prev-init real-prev-init mult-val mod-val)
  `(let ((previous nil))
     (defun ,name ()
       (unless previous
	 (setf previous (if *example* ,ex-prev-init ,real-prev-init)))
       ;; (format t "Example is ~a previous is ~a ~a ~%" *example* previous (if *example* 'FOO 'BAR))
       (do ()
	   (nil)
	 (setf previous (mod (* previous ,mult-val) 2147483647))
	 (if (or (= *part* 1) (= 0 (mod previous ,mod-val)))
	     (return-from ,name previous))))))

(gen gen-a 65 618 16807 4)
(gen gen-b 8921 814 48271 8)

(defun main (args)
  ;; Parse command-line options
  (let ((opts '(("p" :required 0)
		("e" :none nil))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "p" (car arg))
		 (setf *part* (parse-integer (cdr arg))))
		((string= "e" (car arg))
		 (setf *example* t))))
	(setf args new-args)))

  (format t "Running part ~d~%" *part*)
  (format t "USING ~a values~%" (if *example* "EXAMPLE" "ACTUAL"))
  
  (let ((score 0))
    (dotimes (i (if (= *part* 1) (* 40 1000 1000) (* 5 1000 1000)))
      (let ((a (gen-a))
	    (b (gen-b)))
	(when (= (mod a #x10000) (mod b #x10000))
	  ;; (format t "~x ~x~%" a b)
	  (incf score))))
    (format t "score ~a~%" score))
  0)

(sb-ext:exit :code (main sb-ext:*posix-argv*))
