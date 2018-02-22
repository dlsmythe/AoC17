;; sbcl --noinform --load adv17-11.lisp < adv17-11.input

(proclaim '(optimize (speed 3) (safety 0)))
;;(proclaim '(optimize (debug 3) (speed 0) (safety 3)))

(ql:quickload "cl-mathstats" :silent t)
(ql:quickload "sdl2" :silent t)
(ql:quickload "split-sequence" :silent t)

(defparameter *part* 1)

(defconstant +BLACK+ '(  0   0   0))
(defconstant +WHITE+ '(255 255 255))
(defconstant +BLUE+  '(  0   0 255))
(defconstant +GREEN+ '(  0 255   0))
(defconstant +RED+   '(255   0   0))

(defparameter *screen* nil)		; sdl2 screen
(defparameter *fg-color* +RED+)
(defparameter *bg-color* +BLACK+)

(defparameter *SCR-H* 8000)
(defparameter *SCR-W* 6000)

(defparameter *size* '(1280 1024))

(defconstant +hex-width+ 40.0)
(defconstant +close-enough+ .5)

(defconstant +cosd+ (* (/ +hex-width+ 2) (cos (cl-mathstats:degrees->radians 30))))
(defconstant +sind+ (* (/ +hex-width+ 2) (sin (cl-mathstats:degrees->radians 30))))

;; these are in game-coords, not screen-coords
(defparameter *scr-base-x* 0)
(defparameter *scr-base-y* 0)
(defparameter *scr-magnify* 1.0)

(defun round-to (number precision &optional (what #'round))
  (let ((div (expt 10 precision)))
    (coerce (/ (funcall what (* number div)) div) 'float)))

(defun cvt-to-scr (coord)
  (destructuring-bind (x y) coord
    (let ((x (truncate (* x *scr-magnify*)))
	  (y (truncate (* y *scr-magnify*))))
      (decf x *scr-base-x*)
      (decf y *scr-base-y*)
      (incf x (/ (elt *size* 0) 2))
      (setf y (- (/ (elt *size* 1) 2) y))
      (values x y))))

(defun reverse-direction (dir)
  "return the reverse direction for the input direction"
  (cadr (assoc dir '((NORTH SOUTH)
		     (NORTHEAST SOUTHWEST)
		     (SOUTHEAST NORTHWEST)
		     (SOUTH NORTH)
		     (SOUTHWEST NORTHEAST)
		     (NORTHWEST SOUTHEAST)))))

(defun join (separator list)
  "join strings in 'list' using string 'separator'. returns a string."
  (with-output-to-string (out)
    (loop for (element . more) on list
       do (princ element out)
       when more
       do (princ separator out))))

(defconstant +dir-list+ '(NORTH NORTHEAST SOUTHEAST SOUTH SOUTHWEST NORTHWEST))
(defconstant +dir-alist+ '(("n" NORTH) ("ne" NORTHEAST) ("se" SOUTHEAST) ("s" SOUTH) ("sw" SOUTHWEST) ("nw" NORTHWEST)))

;; reads step list
(defun read-input (fname)
  (with-open-file (in fname)
    (let ((buf (make-array (list (file-length in)))))
      (read-sequence buf in)
      (let ((step-list (map 'list (lambda (s) (cadr (assoc s +dir-alist+ :test 'equal)))
			    (split-sequence:split-sequence #\, (string-trim '(#\Newline) (concatenate 'string buf))))))
	(when nil
	  (with-open-file (f "regnerated-input-path.txt" :direction :output :if-exists :supersede)
	    (write-sequence (format nil "~a~%" (join #\, step-list)) f)))
	(remove-if #'null step-list)))))

(defconstant +TILE-HEIGHT+ (* 2 +cosd+))
(defconstant +TILE-WIDTH+ +hex-width+)

(defparameter *tile-array* (make-array '(0) :fill-pointer 0 :adjustable t))

;; Access with slot-value, e.g. (slot-value tile 'x)
(defclass tile ()
  ((id :initarg :id :initform nil)
   (x :initarg :x :initform nil)
   (y :initarg :y :initform nil)
   (north :initarg :NORTH :initform nil)
   (northeast :initarg :NORTHEAST :initform nil)
   (southeast :initarg :SOUTHEAST :initform nil)
   (south :initarg :SOUTHEAST :initform nil)
   (southwest :initarg :SOUTHWEST :initform nil)
   (northwest :initarg :NORTHWEST :initform nil)
   (steps-to-origin :initarg :steps-to-origin :initform nil)
   (color :initarg :color :initform +BLUE+)))

(defgeneric tile-id (tile)
  (:documentation "Return the id of the given tile"))
(defmethod tile-id ((tile tile))
  (slot-value tile 'id))

(defgeneric tile-x (tile)
  (:documentation "Return the x coordinate of the given tile"))
(defmethod tile-x ((tile tile))
  (slot-value tile 'x))

(defgeneric tile-y (tile)
  (:documentation "Return the y coordinate of the given tile"))
(defmethod tile-y ((tile tile))
  (slot-value tile 'y))

(defgeneric tile-neighbor (tile dir)
  (:documentation "Return the id of the tile in the given direction, if known."))
(defmethod tile-neighbor ((tile tile) dir)
  (slot-value tile dir))

(defgeneric tile-set-neighbor (tile dir id)
  (:documentation "Set the tile-id of the the neighbor in the given direction to the given value."))
(defmethod tile-set-neighbor ((tile tile) dir id)
  (setf (slot-value tile dir) id))

(defun dist-between (x0 y0 x1 y1)
  (sqrt (+ (expt (- x0 x1) 2) (expt (- y0 y1) 2))))

(defun dist-between-nodes (src dst)
  (dist-between (tile-x src) (tile-y src) (tile-x dst) (tile-y dst)))

(defun dist-between-nodes-byindex (src dst)
  (dist-between-nodes (elt *tile-array* src) (elt *tile-array* dst)))

(defmethod print-object ((tile tile) stream)
  (let ((neighbors (join #\Space (map 'list (lambda (dir)
					      (let ((n (tile-neighbor tile dir)))
						(if n (format nil "~2,,,' :@a: ~a~%" dir n) "")))
				      +dir-list+)))
	(dist (if (= 0 (tile-id tile)) 0 (dist-between 0 0 (tile-x tile) (tile-y tile)))))
    (format stream "~4:@a: (~8,2f, ~8,2f) dist: ~6,2f ~a~%" (tile-id tile) (tile-x tile) (tile-y tile) dist neighbors)))

(let ((once t))			   ; set to nil to see the format stmt
  (defun tile-coords (cx cy)
    "given the center coordinates of a tile, return a list of its vertices"
    (let ((c-nw (list (- cx +sind+) (+ cy +cosd+)))
	  (c-ne (list (+ cx +sind+) (+ cy +cosd+)))
	  (c-e  (list (+ cx (/ +hex-width+ 2)) cy))
	  (c-se (list (+ cx +sind+) (- cy +cosd+)))
	  (c-sw (list (- cx +sind+) (- cy +cosd+)))
	  (c-w  (list (- cx (/ +hex-width+ 2)) cy)))

      (unless once
	(setf once t)
	(format t "hex: ~A -> ~A~%"
		(list c-nw c-ne c-e c-se c-sw c-w)
		(list (cvt-to-scr c-nw) (cvt-to-scr c-ne) (cvt-to-scr c-e)
		      (cvt-to-scr c-se) (cvt-to-scr c-sw) (cvt-to-scr c-w))))
      (list (cvt-to-scr c-nw) (cvt-to-scr c-ne) (cvt-to-scr c-e)
	    (cvt-to-scr c-se) (cvt-to-scr c-sw) (cvt-to-scr c-w)))))

(defgeneric coords-for-direction (tile dir)
  (:documentation "Given a direction, return the coordinates of the center of the tile a step in that direction."))
(defmethod coords-for-direction ((tile tile) dir)
  (cond ((eql dir 'NORTH)
	 (list (tile-x tile) (+ (tile-y tile) (* +hex-width+ (cos (cl-mathstats:degrees->radians 30))))))

	((eql dir 'NORTHEAST)
	 (list (+ (tile-x tile) (* .75 +hex-width+))
		 (+ (tile-y tile) (* (/ +hex-width+ 2) (cos (cl-mathstats:degrees->radians 30))))))

	((eql dir 'SOUTHEAST)
	 (list (+ (tile-x tile) (* .75 +hex-width+))
		 (- (tile-y tile) (* (/ +hex-width+ 2) (cos (cl-mathstats:degrees->radians 30))))))

	((eql dir 'SOUTH)
	 (list (tile-x tile)
		 (- (tile-y tile) (* +hex-width+ (cos (cl-mathstats:degrees->radians 30))))))

	((eql dir 'SOUTHWEST)
	 (list (- (tile-x tile) (* .75 +hex-width+))
		 (- (tile-y tile) (* (/ +hex-width+ 2) (cos (cl-mathstats:degrees->radians 30))))))

	((eql dir 'NORTHWEST)
	 (list (- (tile-x tile) (* .75 +hex-width+))
		 (+ (tile-y tile) (* (/ +hex-width+ 2) (cos (cl-mathstats:degrees->radians 30))))))

	(t
	 (format t "bad dir ~A~%" dir)
	 (sb-ext:exit :code 1))))

(defun loc-hash-str (x y)
  (format nil "~8,2,,,'0f-~8,2,,,'0f" (round-to x 2) (round-to y 2)))

(let ((next-tile-id 0)
      (tile-hash (make-hash-table :test 'equal)))
  (defun tile-at (x y)
    (let* ((h (loc-hash-str x y))
	   (tile (gethash h tile-hash)))
      (unless tile
	;; (format t "making new tile at (~a ~a)~%" x y)
	(setf tile (make-instance 'tile :id next-tile-id :x x :y y))
	(incf next-tile-id)

	;; hook up any already-existing neighbors
	(dolist (dir +dir-list+)
	  (unless (tile-neighbor tile dir)
	    (destructuring-bind (nx ny) (coords-for-direction tile dir)
	      (let ((ntile (gethash (loc-hash-str nx ny) tile-hash)))
		(when ntile
		  (tile-set-neighbor tile dir (tile-id ntile))
		  (tile-set-neighbor ntile (reverse-direction dir) (tile-id tile)))))))

	;; (format t "New tile: ~a~%" tile)
	(vector-push-extend tile *tile-array*)
	(setf (gethash (loc-hash-str x y) tile-hash) tile))
      tile)))

(defun tile-in-dir (tile dir)
  ;; print 'tile-in-dir({0},{1})'.format(tile['id'],dir)
  (destructuring-bind (x y) (coords-for-direction tile dir)
    (tile-at x y)))

;; ==========================================

;; Start at some tile, and work one tile-location at a time towards tile 0.
;; Keep note of tile locations calculated so far.
;; When you get to tile 0 or a previously hashed value, return up the list
;; of locations back towards the destination and add them all to the hash.
;; Return the final result.
(let ((origin-dist (make-hash-table :test 'equal)))
  (defun steps-to-origin (tileid)
    ;; (format t "steps-to-origin ~A~%" tileid)
    (let ((ns 0)
	  (tile (elt *tile-array* tileid)))
      (if (= 0 tileid)
	  (setf (slot-value tile 'steps-to-origin) 0)
	  (let ((deltas (make-hash-table :size (length +dir-list+)))
		(t0 (elt *tile-array* 0))
		(steps (make-array '(0) :fill-pointer 0 :adjustable t)))
	    (loop for dir in +dir-list+
	       do (setf (gethash dir deltas) (coords-for-direction t0 dir)))
	    ;; (loop for dir in +dir-list+
	    ;;    do (format t "deltas in dir ~a are ~a~%" dir (gethash dir deltas)))
	    
	    ;; (format t "counting steps~%")
	    (let ((minpos-x nil)
		  (minpos-y nil))
	      (do ((cur-x (tile-x tile) minpos-x)
		   (cur-y (tile-y tile) minpos-y))
		  ((or (/= 0 ns) (and (<= (abs cur-x) +close-enough+) (<= (abs cur-y) +close-enough+))))
		;; See if we knew the rest of the distance from here
		(let* ((h (loc-hash-str cur-x cur-y))
		       (hd (gethash h origin-dist)))
		  ;; (format t "ns ~a checking whether location ~A is known~%" ns h)
		  (if hd
		      (setf ns hd)
		      (let ((mindist most-positive-fixnum))

			;; add the new tile to the step array
			;; (format t "adding ~a to the steps list~%" h)
			(vector-push-extend h steps)
		      
			;; for the tile-center in each direction, find which of those positions is
			;; closest to the destination
			;; (format t "looking for best next step...~%")
			(dolist (dir +dir-list+)
			  (destructuring-bind (nx ny) (gethash dir deltas)
			    (let* ((c-x (+ cur-x nx))
				   (c-y (+ cur-y ny))
				   (d (dist-between 0.0 0.0 c-x c-y)))
			    ;; (format t "~a (~A,~A) dist to origin: ~A~%" dir nx ny d)
			    (when (< d mindist)
			      (setf mindist d)
			      (setf minpos-x c-x)
			      (setf minpos-y c-y))))))))))

	    ;; (format t "Adding ~a discovered tiles to the hash~%" (length steps))
	    (dotimes (i (length steps))
	      (incf ns)
	      (let ((h (vector-pop steps)))
		;; (format t "remembering steps from ~A is ~A~%" h ns)
		(setf (gethash h origin-dist) ns)))))
      ns)))

;; ==========================================

;; def children-of(tileid):
;;     ret = []
;;     for dir in ['NORTH','NORTHEAST','SOUTHEAST','SOUTH','SOUTHWEST','NORTHWEST']:
;;         if *tile-array*[tileid][dir]:
;;             ret.append(*tile-array*[tileid][dir])
;;     return ret

;; class BFS:
;;     def --init--(self, verbose=False):
;;         self.open-set = deque([])
;;         self.closed-set = set()
;;         self.verbose = verbose
;;         self.max-path-len = 0
;;         self.max-path-tileid = 0
;;         self.num-nodes-visited = 0

;;     def next-best(self):
;;         return self.open-set.popleft()

;;     def add-children-to-frontier(self, parent):
;;         for child in children-of(parent):
;;             ;; if self.verbose:
;;             ;;     print 'processing child {0}'.format(child)
;;             if child in self.closed-set:
;;                 ;; if self.verbose:
;;                 ;;     print ' already processed'
;;                 continue
;;             if child not in self.open-set:
;;                 ;; child.level = parent.level + 1
;;                 ;; set child's path to current path + parent
;;                 *tile-array*[child]['path'] = *tile-array*[parent]['path']+[parent]
;;                 ;; if self.verbose:
;;                 ;;     print ' adding child {0} to open-set with path {1}'.format(child, *tile-array*[child]['path'])
;;                 self.open-set.append(child)
;;                 sfo = steps-tooorigin(child)
;;                 if sfo > self.max-path-len:
;;                     self.max-path-len = sfo
;;                     self.max-path-tileid = child

;;     def search(self, goalnode):
;;         self.goal-node = goalnode
;;         self.open-set.append(0)
;;         *tile-array*[0]['path'] = []
;;         while len(self.open-set) > 0:
;;             node = self.next-best()
;;             self.num-nodes-visited += 1
;;             if self.verbose:
;;                 print 'VISITING node {0}'.format(node)
;;             ;; here is the test for completion
;;             if node == self.goal-node and not Part == 2:
;;                 return *tile-array*[node]['path'] + [node]
;;             self.add-children-to-frontier(node)
;;             self.closed-set.add(node)
;;         return None

;; class TileBFS(BFS):

;;     def --init--(self, verbose=False):
;;         BFS.--init--(self, verbose)

;;     def next-best(self):
;;         min-node = None
;;         min-val = len(*tile-array*)+1
;;         if self.verbose and len(self.open-set) > 1:
;;             print 'open-set {0}'.format(self.open-set)
;;         for node in self.open-set:
;;             len-to-here = len(*tile-array*[node]['path'])
;;             if self.verbose and len(self.open-set) > 1:
;;                 print 'frontier node {0} len-to-here: {1} min-val: {2} min-node: {3}'.format(node, len-to-here, min-val, min-node)
;;             if len-to-here < min-val:
;;                     min-val = len-to-here
;;                     min-node = node
;;         if self.verbose and len(self.open-set) > 1:
;;             print 'selecting {0} as best frontier node with len {1}'.format(min-node, min-val)
;;         self.open-set.remove(min-node)
;;         return min-node

;; ==========================================

(defconstant +KEY-UP+ 273)
(defconstant +KEY-DOWN+ 274)
(defconstant +KEY-RIGHT+ 275)
(defconstant +KEY-LEFT+ 276)
(defconstant +KEY-KPUP+ 264)
(defconstant +KEY-KPRIGHT+ 262)
(defconstant +KEY-KPDOWN+ 258)
(defconstant +KEY-KPLEFT+ 260)
(defconstant +KEY-KPCENTER+ 261)
(defconstant +KEY-KPMINUS+ 269)
(defconstant +KEY-KPPLUS+ 270)
(defconstant +KEY-Q+ 113)

(defun pan (keycode)
  (let ((pan-amount (+ (/ (elt *size* 0) 4)
		       (min (/ (elt *size* 0) 4) (* (/ (elt *size* 0) 16) (/ 1.(sqrt *scr-magnify*)))))))
    (cond ((or (= keycode +KEY-RIGHT+) (= keycode +KEY-KPRIGHT+))
	   (format t "KEY-RIGHT~%")
	   (incf *scr-base-x* pan-amount))
	  ((or (= keycode +KEY-LEFT+) (= keycode +KEY-KPLEFT+))
	   (format t "KEY-LEFT~%")
	   (decf *scr-base-x* pan-amount))
	  ((or (= keycode +KEY-UP+) (= keycode +KEY-KPUP+))
	   (format t "KEY-UP~%")
	   (incf *scr-base-y* pan-amount))
	  ((or (= keycode +KEY-DOWN+) (= keycode +KEY-KPDOWN+))
	   (format t "KEY-DOWN~%")
	   (decf *scr-base-y* pan-amount))
	  ((= keycode +KEY-KPCENTER+)
	   (setf *scr-base-x* 0)
	   (setf *scr-base-y* 0)
	   (setf *scr-magnify* 1.0))
	  ((= keycode +KEY-KPPLUS+)
	   (setf *scr-magnify* (* *scr-magnify* 1.1)))
	  ((= keycode +KEY-KPMINUS+)
	   (when (> *scr-magnify* .01)
	     (setf *scr-magnify* (* *scr-magnify* .9)))))))

;; (defun draw-tile (tile)
;;   (let ((coords (tile-coords (tile-x tile) (tile-y tile))))
;;     (sdl2:draw-polygon *screen* (tile-color tile) coords (if (> *scr-magnify* .3) 2 0))
;;     (when (> *scr-magnify* .3)
;;       (sdl2:font-set-bold t)
;;       (let* ((lbl (sld2:font-render (format nil "~A" (tile-id tile)) t *fg-color* *bg-color*))
;; 	     (rect (sdl2:get-rect lbl)))
;; 	(setf rect.center (cvt-to-scr (tile-x tile) (tile-y tile)))
;;         (sdl2:blit screen lbl rect)))))

;; =================================================================

(defun main (args)
  (declare (ignore args))

  ;; (sdl2:init())
  ;; (setf *screen* (sdl2:display-set-mode *size*))

  ;; (sdl2:display-set-caption "hex tile display for AoC2017-11")

  ;; (setf *font* (sdl2:font-font nil (truncate (* +hex-width+ .4))))

  ;; ==========================================

  (let ((child-steps (read-input "adv17-11.input"))
	(tile (tile-at 0.0 0.0))    ; First, add the tile we start on.
	(max-dist 0)
	(max-dist-tile 0))
    
    (format t "The child took ~A steps~%" (length child-steps))

    ;; walk the child's steps to populate the known tiles.
    
    (dolist (step child-steps)
      ;; (format t "step ~a~%" step)
      (setf tile (tile-in-dir tile step))
      (let ((sto (steps-to-origin (tile-id tile))))
	(when (> sto max-dist)
	  (setf max-dist sto)
	  (setf max-dist-tile (tile-id tile)))
	;; (format t "tile ~a is ~a steps from the origin~%" (tile-id tile) sto)
	(setf (slot-value tile 'steps-to-origin) sto)))
    
    (format t "wrapping up~%")
    (let ((dest-tile-id (tile-id tile)))
      (format t "child ended on tile ~A at (~A,~A) (first tile is labeled 0)~%"
	      dest-tile-id (tile-x tile) (tile-y tile)))
    (format t "There are ~A unique tiles.~%" (length *tile-array*))
    (format t "child ended ~A steps from the origin~%" (slot-value tile 'steps-to-origin))

    (format t "Longest distance from the start was ~a at node ~a~%" max-dist max-dist-tile)
  
    ;; Color the tiles chosen for the shortest path in GREEN, others in BLUE
    ;; for id in path:
    ;;     t = *tile-array*[id]
    ;;     if t:
    ;;         t['color'] = GREEN
    ;;     else:
    ;;         (format t "tile ~A on path but not in *tile-array*?~%" id)

    ;; Loop until the user clicks the close button or types 'q'.
  
    ;; (do ((done nil)
    ;;      (clock (sdl2:time-Clock)))

    ;;     (done)
    
    ;;   ;; This limits the while loop to a max of 10 times per second.
    ;;   ;; Leave this out and we will use all CPU we can.
    ;;   (sdl2:clock-tick 10)
    
    ;;   (loop for event in (sdl2:event-get) ; User did something
    ;; 	 (cond ((= event-type sdl2:QUIT) ; If user clicked close
    ;; 		(setf done t)) ; Flag that we are done so we exit this loop

    ;; 	       ;; key.--dict--: {'scancode': 113, 'key': 276, 'mod': 0}
    ;; 	       ((= event-type (sdl2:KEYUP))
    ;; 		;; (format t "key {0}'.format(event.--dict--)
    ;; 		(if (= event.__dict__['key'] KEY-Q)
    ;; 		    (setf done t)
    ;; 		    (pan event.--dict--['key'])))))

    ;;   ;; Clear the screen and set the screen background
    ;;   (sdl2:fill *screen* BLACK)

    ;;   (loop for t across *tile-array* do (draw-tile t))
    
    ;;   ;; Go ahead and update the screen with what we've drawn.
    ;;   ;; This MUST happen after all the other drawing commands.
    ;;   (sdl2:display-flip))
  
    ;; ;; Be IDLE friendly
    ;; (sdl2:quit())
  
    0))

(sb-ext:exit :code (main sb-ext:*POSIX-ARGV*))
