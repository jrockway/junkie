(in-package #:junkie)

(defclass requirement ()
  ((name :initarg :name :reader name)
   (documentation :initarg :documentation :initform "")))

(defmacro define-requirement (name &optional (documentation ""))
  `(defparameter ,name
     (make-instance 'requirement :name ',name :documentation ,documentation)
     ,documentation))

(defclass with-provides-requires (standard-class)
  ((slot-requirement-table :initform (make-hash-table :test 'equal)
                           :accessor slot-requirement-table)
   (class-provision-list :initform () :accessor class-provides)))


;;; define metaclass compatability.  eventually all these will be T,
;;; but i am too lazy to implement those right now.
(defmethod validate-superclass
    ((class with-provides-requires) (superclass standard-class))
  t)
(defmethod validate-superclass
    ((class standard-class) (superclass with-provides-requires))
  nil)
(defmethod validate-superclass
    ((class with-provides-requires) (superclass with-provides-requires))
  nil)

(defgeneric add-requirement (class slot requirement)
  (:documentation "Update the requirements for CLASS to note that SLOT requires REQUIREMENT"))

(defmethod add-requirement
    ((class with-provides-requires) (slot slot-definition)
     (requirement requirement))
  (let ((initargs (slot-definition-initargs slot)))
    (when (= (length initargs) 0)
      (error "Cannot apply a requirement to a slot with no initargs"))
    (when (> (length initargs) 1)
      (error "Cannot apply a requirement to a slot with more than one initarg (BUG)")))
  (pushnew requirement (gethash (slot-definition-name slot)
                                (slot-requirement-table class))))

(defmethod add-requirement
    ((class with-provides-requires) (slot string)
     (requirement requirement))
  (let* ((slot-definitions (class-slots class))
         (found-slot
          (or (iterate (for s in slot-definitions)
                       (when (string-equal (slot-definition-name s) slot)
                         (leave s)))
              (error (format nil "No slot named ~A in ~A" slot (class-name class))))))
    (add-requirement class found-slot requirement)))


(defgeneric class-requires (class)
  (:documentation "Return a set of all requirements needed to build CLASS"))

(defmethod class-requires ((class standard-class)) nil)
(defmethod class-requires ((class with-provides-requires))
  (let (set)
    (iter (for (slot requirements-list) in-hashtable (slot-requirement-table class))
          (iter (for requirement in requirements-list)
                (pushnew requirement set)))
    set))

(defgeneric add-provision (place provision)
  (:documentation "Update the provision table to indicate that PLACE provides PROVISION"))

(defmethod add-provision ((class with-provides-requires) (provision requirement))
  (pushnew provision (class-provides class)))

(defclass action-area ()
  ((requires-table :initform () :accessor requires-table)
   (class-provides-table :initform () :accessor class-provides-table)
   (slot-provides-table :initform () :accessor slot-provides-table)))

(defgeneric insert (area thing)
  (:documentation "Insert THING into the action-area AREA"))

;; insert a class
(defmethod insert ((area action-area) (thing with-provides-requires))
  )

;; insert an instance
(defmethod insert ((area action-area) (thing standard-object))
  )

;; insert a bare provider
(defmethod insert ((area action-area) (thing cons)
                   &aux (provides (car thing)) (value (cdr thing)))
  )
