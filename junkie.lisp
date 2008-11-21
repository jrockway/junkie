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

(defmethod add-requirement ((class symbol) slot requirement)
  (add-requirement (find-class class) slot requirement))

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

;; note to self: i just realized that we probably never need
;; class-requirements; we always want (slot . requirement) tuples
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

(defgeneric satisfy-requirements-1 (area provision))
(defmethod satisfy-requirements-1 ((area action-area) (provision requirement))
  (gethash (name provision) (provision-table area)))

(defgeneric satisfy-requirements (area requirements))
(defmethod satisfy-requirements ((area action-area) (requirements list))
  (reduce #'intersection
          (mapcar
           (lambda (requirement) (satisfy-requirements-1 area requirement))
           requirements)))

;; at some point, we may want to rewrite this to use the usual
;; slot-definition-initfunction

(defgeneric build-initargs-for (class area &optional ignorable-initargs))
(defmethod build-initargs-for
    ((class with-provides-requires) (area action-area) &optional ignorable-initargs)
  (iter
    (for (slot-name requirements)
         in-hashtable (slot-requirement-table class))
    (let* ((slot (find-slot class slot-name))
           (initarg-keyword (car (slot-definition-initargs slot))))
      (when (not (position initarg-keyword ignorable-initargs :test 'eq))
        (nconcing
         (cons initarg-keyword (satisfy-requirements area requirements)))))))

(defmethod make-instance ((class with-provides-requires) &rest initargs)
  (destructuring-bind (&key ((:from-action-area area)) &allow-other-keys) initargs
    (when area
        (setf initargs
              (append (build-initargs-for class area initargs)
                      (iter (for (key value) on initargs by #'cddr)
                            (when (not (eq key :from-action-area))
                              (nconcing (list key value))))))))
  (apply #'call-next-method (cons class initargs)))
