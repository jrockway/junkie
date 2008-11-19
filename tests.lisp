(in-package #:junkie)

(def-suite junkie)
(in-suite junkie)

(defclass test-requires-foo ()
  ((foo :initarg :foo)) ; requires "foo"
  (:metaclass with-provides-requires))

(defclass test-provides-foo ()
  ((foo-source :initarg :foo-source :initform 42)) ; provides "foo"
  (:metaclass with-provides-requires))

(finalize-inheritance (find-class 'test-requires-foo))
(finalize-inheritance (find-class 'test-provides-foo))

(define-requirement foo)

(test with-provides-requires
  (let ((requires-foo (find-class 'test-requires-foo)))
    ;; cleanup from previous results
    (setf (slot-requirement-table requires-foo)
          (make-hash-table :test 'equal))

    (is (= 0 (length (class-requirements requires-foo)))
        "No requirements to build test-requires-foo yet")
    (finishes
      (add-requirement requires-foo "foo" foo)
      "adding the 'foo' requirement lives")
    (is (= 1 (length (class-requirements requires-foo)))
        "requirement seen via class-requirements")
    (is (eq (car (class-requirements requires-foo))
            foo)
        "requirement added is foo")
    (finishes
      (add-requirement requires-foo "foo" foo)
      "adding the 'foo' requirement again is OK")
    (is (= 1 (length (class-requirements requires-foo)))
        "but we don't add it twice")))

;(test action-area
;  (let (a)
;    (finishes (setf a (make-instance 'action-area)) "make action area")))
