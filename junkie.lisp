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

(defun find-slot (class slot-name)
  (or (iter (for s in (class-slots class))
            (when (string-equal (slot-definition-name s) slot-name)
              (leave s)))
      (error (format nil "No slot named ~A in ~A" slot-name (class-name class)))))

(defmethod add-requirement ((class with-provides-requires) (slot-name string)
                            (requirement requirement))
  (add-requirement class (find-slot class slot-name) requirement))


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
  ((provision-table :initform (make-hash-table) :accessor provision-table)))

(defgeneric insert (area thing)
  (:documentation "Insert THING into the action-area AREA"))

;; insert a class
(defmethod insert ((area action-area) (class with-provides-requires))
  (iter (for provision in (class-provides class))
        (insert area (cons provision class))))

;; insert an instance
(defmethod insert ((area action-area) (thing standard-object))
  (iter (for provision in (class-provides (class-of thing)))
        (insert area (cons provision thing))))

;; insert a bare provider
(defmethod insert ((area action-area) (spec cons)
                   &aux (provision (car spec)) (thing (cdr spec)))
  (declare (type requirement provision))
  (pushnew thing (gethash (name provision) (provision-table area))))

(defgeneric find-things-providing-1 (area provision))
(defmethod find-things-providing-1 ((area action-area) (provision requirement))
  (gethash (name provision) (provision-table area)))

(defgeneric find-things-providing (area requirements))
(defmethod find-things-providing ((area action-area) (requirements list))
  (declare (type (cons requirement) requirements))
  (reduce #'intersection
          (mapcar
           (lambda (requirement) (find-things-providing-1 area requirement))
           requirements)))

(defgeneric obtain-instance (class area)
  (:documentation "Create an instance of CLASS using the action-area AREA."))

(defmethod obtain-instance ((class symbol) (area action-area))
  (obtain-instance (find-class class) area))

(defmethod obtain-instance ((class with-provides-requires) (area action-area))
  ;; note to self: i just realized that we probably never need
  ;; class-requirements; we always want (slot . requirement) tuples
  (apply #'make-instance
         (cons class
               (iter
                 (for (slot-name requirements) in-hashtable (slot-requirement-table class))
                 (nconcing
                  (list (car (slot-definition-initargs (find-slot class slot-name)))
                        ;; TODO: we need to recurse here when it's a
                        ;; class instead of an instance
                        (car (find-things-providing area requirements))))))))
