;;; sbcl --noinform --load adv17-18.lisp [-v n]
;;;  -v n  set verbosity to level n
;;;
;;; New here:
;;; - sockets
;;; - processes
;;; - inter-process synchronization

;;(proclaim '(optimize (speed 3) (safety 0)))
(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "getopt" :silent t)
(ql:quickload "split-sequence" :silent t)
(ql:quickload :exit-hooks :silent t)

(defclass insn ()
  ((opcode :initarg :opcode :initform nil :accessor insn-op)
   (dst :initarg :dst :initform nil :accessor insn-dst)
   (dst-is-constant :initarg :dst-is-constant :initform nil :accessor dst-is-constant)
   (src :initarg :src :initform nil :accessor insn-src)
   (src-is-constant :initarg :src-is-constant :initform nil :accessor src-is-constant))
  (:documentation ""))

(defparameter *verbose* nil)
(defparameter *program-id* 0)
(defparameter *prog* (make-array '(0) :fill-pointer 0 :adjustable t))
(defparameter *trace* nil)

(defparameter *regs* (make-array '(26) :initial-element nil))
(defparameter *regs-set* (make-array '(26) :initial-element nil))
(defparameter *freqval* 0)
(defparameter *freqval-set* nil)

;; int pfds[2][2];
;; int pids[2];
;; char *sem_name = "/adv17-18";
;; sem_t *child_counter_semaphore;

(defclass reg ()
  ((value :initarg :value :initform 0 :accessor reg-value)
   (been-set :initarg :been-set :initform nil :accessor been-set)))

(defmethod print-object ((reg reg) stream)
  (with-slots (value been-set) reg
    (format t "#<value: ~a been-set: ~a>" value been-set)))

(defgeneric set-reg (reg val)
  (:documentation ""))

(defmethod set-reg ((reg reg) val)
  (with-slots (value been-set) reg
    (setf been-set t)
    (setf value val)))

(defmethod set-reg ((regnum fixnum) val)
  (format t "set-reg(fixnum ~a) to ~a~%" regnum val)
  (let ((reg (aref *regs* regnum)))
    (cond ((null reg)
	   (format t "Making a new reg~%")
	   (setf (aref *regs* regnum) (make-instance 'reg :value val :been-set t)))
	  (t
	   (setf (slot-value 'been-set reg) t)
	   (setf (slot-value 'value reg) val))))
  (format t "reg ~a = ~a~%" regnum (reg-value (aref *regs* regnum))))

(defgeneric get-reg (reg)
  (:documentation ""))

(defmethod get-reg ((reg reg))
  (slot-value 'value reg))

(defmethod get-reg ((regnum fixnum))
  (let ((reg (aref *regs* regnum)))
    ;; (when (or (null reg) (not (slot-value 'been-set reg)))
    ;;   (format t "read of uninitialized value"))
    (if reg (slot-value 'value reg) 0)))

(defun get-reg-byname (name)
  (get-reg (- (char-code name) (char-code #\a))))

(defgeneric reg-setp (reg)
  (:documentation ""))

(defmethod reg-setp ((reg reg))
  (format t "reg-setp(reg) ~a~%" reg)
  (slot-value 'been-set reg))

(defmethod reg-setp ((regnum fixnum))
  (let ((reg (aref *regs* regnum)))
    (format t "reg-setp(fixnum) ~a: reg: ~a~%" regnum reg)
    (when reg
      (format t "  value: ~a~%" (reg-value reg)))
    (format t "reg-setp ~a: value ~a been-set ~a~%" regnum (if reg (slot-value 'value reg) nil) (if reg (slot-value 'been-set reg) nil))
    (format t "reg-setp: ~a~%" reg)
    (format t "reg-setp ~a? (~a) ~a~%" regnum reg (if reg (slot-value 'been-set reg) nil))
    (if reg (slot-value 'been-set reg) nil)))
    
(defparameter *only-print-valid-regs* t)
(defun print-regs ()
  (format t "PRINT-REGS~%")
  (unless (and *only-print-valid-regs* (not *freqval-set*))
    (format t "p~d:   freqval: ~a~a~%" *program-id* *freqval* (if *freqval-set* "" "(never set)")))
  (loop for i from 0 below 26
     do (progn
	  (format t "printing reg ~a~%" i)
	  (when (or (not *only-print-valid-regs*) (reg-setp i))
	    (format t "reg is ~a~%" (code-char (+ (char-code #\a) i)))
	    (format t "~d:   reg ~a: ~a~a~%" *program-id* (code-char (+ (char-code #\a) i))
		    (get-reg i) (if (reg-setp i) "" "(never set)")))))
  (format t "PRINT-REGS COMPLETE~%"))

;; Returns pair, first is regindex/constvalue, second is boolean: "first-is-constant?"
(defun read-val (arg)
  (let ((cc (- (char-code (char arg 0)) (char-code #\a))))
    (if (and (>= cc 0) (< cc 26))
	(values cc nil)
	(values (parse-integer arg) t))))

(defun make-insn (line fields)
  (let ((opc (elt fields 0)))
    ;;(format t "line ~A: ~A ~A ~A~%" line a1 a2 a3)
    (multiple-value-bind (dst dst-is-const) (read-val (elt fields 1))
      (cond ((string= opc "set")
	     (multiple-value-bind (src src-is-const) (read-val (elt fields 2))
	       (make-instance 'insn :opcode 'OPC-SET
			      :dst dst :dst-is-constant dst-is-const
			      :src src :src-is-constant src-is-const)))
	    ((string= opc "add")
	     (multiple-value-bind (src src-is-const) (read-val (elt fields 2))
	       (make-instance 'insn :opcode 'OPC-ADD
			      :dst dst :dst-is-constant dst-is-const
			      :src src :src-is-constant src-is-const)))
	    ((string= opc "mul")
	     (multiple-value-bind (src src-is-const) (read-val (elt fields 2))
	       (make-instance 'insn :opcode 'OPC-MUL
			      :dst dst :dst-is-constant dst-is-const
			      :src src :src-is-constant src-is-const)))
	    ((string= opc "mod")
	     (multiple-value-bind (src src-is-const) (read-val (elt fields 2))
	       (make-instance 'insn :opcode 'OPC-MOD
			      :dst dst :dst-is-constant dst-is-const
			      :src src :src-is-constant src-is-const)))
	    ((string= opc "snd")
	     (make-instance 'insn :opcode 'OPC-SND
			    :dst dst :dst-is-constant dst-is-const))
	    ((string= opc "rcv")
	     (make-instance 'insn :opcode 'OPC-RCV
			    :dst dst :dst-is-constant dst-is-const))
	    ((string= opc "jgz")
	     (multiple-value-bind (src src-is-const) (read-val (elt fields 2))
	       (make-instance 'insn :opcode 'OPC-JGZ
			      :dst dst :dst-is-constant dst-is-const
			      :src src :src-is-constant src-is-const)))
	    (t
	     (format t "Bogus instruction: ~A~%" line)
	     (sb-ext:exit :code 1))))))

(defmethod print-object ((insn insn) stream)
  (with-slots (opcode dst dst-is-constant src src-is-constant) insn
    (format stream "#<insn: [~a] ~a ~a>" *program-id* opcode 
	    (if dst-is-constant dst (code-char (+ (char-code #\a) dst))))
    (unless (or (eql opcode 'OPC-SND) (eql opcode 'OPC-RCV))
      (format stream " ~a" (if src-is-constant src (code-char (+ (char-code #\a) src)))))))

(defun print-program ()
  (loop
     for ins across *prog*
     for pc from 0
     do (format t "[~a] ~a: ~a~%" *program-id* pc ins))
  (format t "~a: ======================~%" *program-id*))
  
(defun read-program (filename)
  (with-open-file (in filename)
    (let ((prog (loop for line = (read-line in nil)
		   while line
		   collect (let ((fields (split-sequence:split-sequence #\space (string-trim '(#\Newline) line))))
			     (make-insn line fields)))))
      (let ((pv (make-array (list (length prog)) :initial-contents prog)))
	pv))))

(defun run-prog ()
  (let ((dst nil)
	(val nil))
    (format t "program ~a (~a) running~%" *program-id* (get-reg-byname #\p))
    (do ((pc 0)
	 (simtime 0))
	((or (< pc 0) (>= pc (length *prog*))) (format t "~a: Program terminated at time ~a (pc=~a)~%" *program-id* simtime pc))
      (let ((insn (aref *prog* pc)))
	(when *trace*
	  (format t "~a: => [~a] ~a: ~a~%" *program-id* simtime pc insn)
	  (print-regs))
	(cond ((eql 'OPC-SET (insn-op insn))
	       ;; (assert (not (dst-is-constant insn)))
	       (setf val (if (src-is-constant insn) (insn-src insn) (get-reg (insn-src insn))))
	       (set-reg (insn-dst insn) val)
	       (incf pc))
	      ((eql 'OPC-ADD (insn-op insn))
	       ;; (assert (not (dst-is-constant insn)))
	       (setf val (if (src-is-constant insn) (insn-src insn) (get-reg (insn-src insn))))
	       (incf (insn-dst insn) val)
	       (incf pc))
	      ((eql 'OPC-MUL (insn-op insn))
	       ;; (assert (not (dst-is-constant insn)))
	       (setf val (if (src-is-constant insn) (insn-src insn) (get-reg (insn-src insn))))
	       (setf (insn-dst insn) (* (insn-dst insn) val))
	       (incf pc))
	      ((eql 'OPC-MOD (insn-op insn))
	       ;; (assert (not (dst-is-constant insn)))
	       (setf val (if (src-is-constant insn) (insn-src insn) (get-reg (insn-src insn))))
	       (setf (insn-dst insn) (mod (insn-dst insn) val))
	       (incf pc))
	      ((eql 'OPC-JGZ (insn-op insn))
	       (setf dst (if (dst-is-constant insn) (insn-dst insn) (get-reg (insn-dst insn))))
	       (setf val (if (src-is-constant insn) (insn-src insn) (get-reg (insn-src insn))))
	       (incf pc (if (> dst 0) val 1)))

	      ((eql 'OPC-SND (insn-op insn))
	       (setf dst (if (dst-is-constant insn) (insn-dst insn) (get-reg (insn-dst insn))))
	       ;; #if PART == 1
	       (setf *freqval* dst)
	       (setf *freqval-set* t)
	       (incf pc))
	      ;; #else
	      ;; 	    {
	      ;; 		char buf[80];
	      ;; 		int len = sprintf(buf, "%ld\n", reg), wlen;
	      ;; 		static int written-so-far = 0;
	      ;; 		written_so_far++;
	      ;; 		printf("%d: SND[%d] %ld (%d)\n", program_id, pfds[1-program_id][1], reg, written_so_far);
	      ;; #if USE_PIPES
	      ;; 		if (-1 == (wlen = write(pfds[1-program_id][1], buf, len))) {
	      ;; 		    err(1, "write");
	      ;; 		}
	      ;; #endif
	      ;; #if USE_LOCAL_SOCKETS
	      ;; 		struct msghdr mhdr;
	      ;; 		struct iovec iov;
	      ;; 		memset(&mhdr, 0, sizeof mhdr);
	      ;; 		iov.iov_base = buf;
	      ;; 		iov.iov-len = len;
	      ;; 		mhdr.msg-iov = &iov;
	      ;; 		mhdr.msg-iovlen = 1;
	      ;; 		ssize-t ret;
	      ;; 		ret = sendmsg(pfds[1-program-id][1], &mhdr, 0);
	      ;; 		if (-1 == ret) {
	      ;; 		    err(1, "sendmsg");
	      ;; 		}
	      ;; 		wlen = ret;
	      ;; #endif
	      ;; 		if (wlen != len) {
	      ;; 		    err(1, "short write");
	      ;; 		}
	      ;; 	    }
	      ;; #endif
	      ;; 	    pc++;
	      ;; 	    break;

	      ((eql 'OPC-RCV (insn-op insn))
	       ;; #if PART == 1
	       (setf dst (if (dst-is-constant insn) (insn-dst insn) (get-reg (insn-dst insn))))
	       (when (/= dst 0)
		 (format t "~a: Recovered frequency: ~a~a~%" *program-id* *freqval* (if *freqval-set* "" " (never set)"))
		 (return-from run-prog)))
	      ;; #else
	      ;; 	    {
	      ;; 		char buf[80], *p;
	      ;; 		int len;
	      ;; 		static int read-so-far = 0;

	      ;; 		if (-1 == sem-wait(child-counter-semaphore)) {
	      ;; 		    err(1, "p%d: sem-wait()", program-id);
	      ;; 		}
	      ;; #if USE-PIPES
	      ;; 		if (-1 == (len = read(pfds[program-id][0], buf, sizeof buf))) {
	      ;; 		    err(1, "read");
	      ;; 		}
	      ;; #endif
	      ;; #if USE-LOCAL-SOCKETS
	      ;; 		struct msghdr mhdr;
	      ;; 		struct iovec iov;
	      ;; 		memset(&mhdr, 0, sizeof mhdr);
	      ;; 		iov.iov-base = buf;
	      ;; 		iov.iov-len = sizeof buf;
	      ;; 		mhdr.msg-iov = &iov;
	      ;; 		mhdr.msg-iovlen = 1;
	      ;; 		ssize-t ret;
	      ;; 		ret = recvmsg(pfds[program-id][0], &mhdr, 0);
	      ;; 		if (-1 == ret) {
	      ;; 		    err(1, "recvmsg");
	      ;; 		}
	      ;; 		len = ret;
	      ;; #endif
	      ;; 		if (-1 == sem-post(child-counter-semaphore)) {
	      ;; 		    err(1, "p%d: sem-post()", program-id);
	      ;; 		}
	      ;; 		val = strtoll(buf, NULL, 0);
	      ;; 		p = strchr(buf, '\n');
	      ;; 		if (p) {
	      ;; 		    *p = '\0';
	      ;; 		    len--;
	      ;; 		}
	      ;; 		read-so-far++;
	      ;; 		printf("%d: RCV[%d] %d bytes (%d) = %ld\n", program-id, pfds[program-id][0], len, read-so-far, val);
	      ;; 		set-reg(prog[pc].reg, val);
	      ;; 	    }
	      ;; #endif
	      ;; 	    pc++;
	      ;; 	    break;
	      (t
	       (format t "Bogus opcode~%")
	       (sb-ext:exit :code 1)))
	(format t "insn execution complete~%")))))

(defun cleanup ()
    ;; if (child-counter-semaphore) {
    ;; 	sem-close(child-counter-semaphore);
    ;; 	sem-unlink(sem-name);
    ;; }
    ;; kill(pids[0], SIGTERM);
    ;; kill(pids[1], SIGTERM);
  (format t "Cleanup complete.~%"))

(defun main (args)
  (let ((do-print nil))

    ;; Parse command-line options
    (let ((opts '(("v" :required nil)
		  ("l" :none nil)
		  ("t" :none nil))))
      (multiple-value-bind (new-args vals) (getopt:getopt args opts)
	(dolist (arg vals)
	  (cond ((string= "v" (car arg))
		 (setf *verbose* (parse-integer (cdr arg))))
		((string= "l" (car arg))
		 (setf do-print t))
		((string= "t" (car arg))
		 (setf *trace* t))))
	(setf args new-args)))

    (setf *prog* (read-program "adv17-18.input"))
    (format t "proglen ~d~%" (length *prog*))
    (if do-print
	(print-program))

    (run-prog)

    (exit-hooks:add-exit-hook #'cleanup)
    
;;     child-counter-semaphore = sem-open(sem-name, O-CREAT|O-RDWR, 0777, 2);
;;     if (SEM-FAILED == child-counter-semaphore) {
;; 	err(1, "sem-open(%s)", sem-name);
;;     }

;; #if USE-PIPES
;;     if (-1 == pipe2(pfds[0], O-DIRECT)) {
;; 	err(1, "pipe(0)");
;;     }
;;     if (-1 == pipe2(pfds[1], O-DIRECT)) {
;; 	err(1, "pipe(1)");
;;     }
;; #endif
;; #if USE-LOCAL-SOCKETS
;;     if (-1 == socketpair(AF-LOCAL, SOCK-DGRAM, 0, pfds[0])) {
;; 	err(1, "socketpair(0)");
;;     }
;;     if (-1 == socketpair(AF-LOCAL, SOCK-DGRAM, 0, pfds[1])) {
;; 	err(1, "socketpair(1)");
;;     }
;; #endif
;;     printf("pipe fds: (%d,%d) (%d,%d)\n", pfds[0][0], pfds[0][1], pfds[1][0], pfds[1][1]);
;; #if USE-PIPES
;;     {
;; 	int val;
;; 	printf("IN PARENT\n");
;; 	if (-1 == fcntl(pfds[1][1], F-GETPIPE-SZ, &val)) {
;; 	    err(1, "fcntl(F-GETPIPE-SZ)");
;; 	}
;; 	printf("Pipe capacity is %d bytes\n", val);
;; 	if (-1 == fcntl(pfds[1][1], F-GETFD, &val)) {
;; 	    err(1, "fcntl(F-GETFD)");
;; 	}
;; 	printf("Descriptor flags: %#x\n", val);
;; 	if (-1 == fcntl(pfds[1][1], F-GETFL, &val)) {
;; 	    err(1, "fcntl(F-GETFL)");
;; 	}
;; 	printf("File-status flags: %#x\n", val);
;; 	//exit(0);
;;     }
;; #endif
    
;;     pids[0] = fork();
;;     if (-1 == pids[0]) {
;; 	err(1, "fork(0)");
;;     }
;;     if (0 == pids[0]) {
;; 	close(pfds[0][1]);
;; 	close(pfds[1][0]);
;; #if USE-PIPES
;;     {
;; 	int val;
;; 	printf("IN CHILD\n");
;; 	if (-1 == fcntl(pfds[1][1], F-GETPIPE-SZ, &val)) {
;; 	    err(1, "fcntl(F-GETPIPE-SZ)");
;; 	}
;; 	printf("Pipe capacity is %d bytes\n", val);
;; 	if (-1 == fcntl(pfds[1][1], F-GETFD, &val)) {
;; 	    err(1, "fcntl(F-GETFD)");
;; 	}
;; 	printf("Descriptor flags: %#x\n", val);
;; 	if (-1 == fcntl(pfds[1][1], F-GETFL, &val)) {
;; 	    err(1, "fcntl(F-GETFL)");
;; 	}
;; 	printf("File-status flags: %#x\n", val);
;; 	//exit(0);
;;     }
;; #endif
;; 	set-reg('p'-'a', 0);
;; 	program-id = 0;
;; 	run-program();
;; 	exit(0);
;;     }

;;     pids[1] = fork();
;;     if (-1 == pids[1]) {
;; 	err(1, "fork(1)");
;;     }
;;     if (0 == pids[1]) {
;; 	close(pfds[0][0]);
;; 	close(pfds[1][1]);
;; 	set-reg('p'-'a', 1);
;; 	program-id = 1;
;; 	run-program();
;; 	exit(0);
;;     }

;;     close(pfds[0][0]);
;;     close(pfds[0][1]);
;;     close(pfds[1][0]);
;;     close(pfds[1][1]);

;;     int dead-children = 0;
;;     while (dead-children < 2) {
;; 	int sts, pid;
;; 	if (-1 == (pid = waitpid(-1, &sts, WNOHANG))) {
;; 	    err(1, "wait");
;; 	}
;; 	if (0 == pid) {
;; 	    int semval;
;; 	    // check for deadlock
;; 	    if (-1 == sem-getvalue(child-counter-semaphore, &semval)) {
;; 		err(1, "sem-getvalue");
;; 	    }
;; 	    if (0 == semval) {
;; 		static time-t when-first-noticed;
;; 		if (0 == when-first-noticed) {
;; 		    when-first-noticed = time(0);
;; 		} else if (time(0) - when-first-noticed > 5) {
;; 		    errx(0, "Deadlock detected");
;; 		}
;; 	    }
;; 	    continue;
;; 	}
;; 	dead-children++;
;; 	if (WIFEXITED(sts)) {
;; 	    printf("Child %d exited with status %d\n", pid, WEXITSTATUS(sts));
;; 	} else if (WIFSIGNALED(sts)) {
;; 	    printf("Child %d exited from signal %d\n", pid, WTERMSIG(sts));
;; 	} else {
;; 	    printf("Child %d exited for an unknown reason\n", pid);
;; 	}
;; 	sleep(1);
;;     }

  0))

(sb-ext:exit :code (main sb-ext:*posix-argv*))
