(defpackage :dls
  (:use :common-lisp)
  (:export :item-set :set-contains :set-add :set-union :set-subtract :set-pop :set-empty))

(in-package :dls)

(defclass item-set ()
  ((memberlist :initarg :initlist :initform nil :accessor members)))

(defgeneric set-contains (item-set item)
  (:documentation "return when item is a member of the set"))
(defmethod set-contains ((item-set item-set) item)
  (with-slots (memberlist) item-set
  (member item memberlist)))

(defgeneric set-add (item-set item)
  (:documentation "Add given item to the set."))
(defmethod set-add ((item-set item-set) item)
  (with-slots (memberlist) item-set
    (setf memberlist (adjoin item memberlist))))

(defgeneric set-union (item-set item-set)
  (:documentation "Add the unique elements of setb to seta."))
(defmethod set-union ((seta item-set) setb)
  (with-slots (memberlist) seta
    (setf memberlist (union memberlist (members setb)))))

(defgeneric set-subtract (seta setb)
  (:documentation "Remove elements from seta that are in setb"))
(defmethod set-subtract ((seta item-set) setb)
  (with-slots (memberlist) seta
    (setf memberlist (mapcar (lambda (x) (remove x memberlist)) (members setb)))))

(defgeneric set-pop (item-set)
  (:documentation "Returns a member of the set, or nil if it is empty."))
(defmethod set-pop ((item-set item-set))
  (with-slots (memberlist) item-set
    (pop memberlist)))

(defgeneric set-empty (item-set)
  (:documentation "Predicate."))
(defmethod set-empty ((item-set item-set))
  (with-slots (memberlist) item-set
    (null memberlist)))
