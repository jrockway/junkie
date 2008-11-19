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

(defun cleanup-classes ()
  ;; cleanup from previous runs
  (iter (for class in '(test-requires-foo test-provides-foo))
        (setf (slot-requirement-table (find-class class))
              (make-hash-table :test 'equal))
        (setf (class-provides (find-class class)) nil)))

(define-requirement foo)

(test dependency-tracking-metaclass
  "tests to make sure the with-provides-requires metaclass works"
  (cleanup-classes)

  (let ((requires-foo (find-class 'test-requires-foo))
        (provides-foo (find-class 'test-provides-foo)))

    ;; test requirements
    (is (= 0 (length (class-requires requires-foo)))
        "No requirements to build test-requires-foo yet")
    (finishes
      (add-requirement requires-foo "foo" foo)
      "adding the 'foo' requirement lives")
    (is (= 1 (length (class-requires requires-foo)))
        "requirement seen via class-requires")
    (is (eq (car (class-requires requires-foo))
            foo)
        "requirement added is foo")
    (finishes
      (add-requirement requires-foo "foo" foo)
      "adding the 'foo' requirement again is OK")
    (is (= 1 (length (class-requires requires-foo)))
        "but we don't add it twice")

    (is (= 0 (length (class-provides requires-foo)))
        "make sure this doesn't provide anything")

    ;; test provisions
    (finishes (add-provision provides-foo foo)
              "adding provision lives")
    (is (= 1 (length (class-provides provides-foo)))
        "provision added ok")
    (is (= 0 (length (class-requires provides-foo)))
        "no requirements")
    (finishes (add-provision provides-foo foo)
              "adding same provision again lives")
    (is (= 1 (length (class-provides provides-foo)))
        "provision not added twice")))

(test action-area-management
  "test insertion, etc. of the action area"
  (cleanup-classes)
  (let (a
        (provides-foo-class (find-class 'test-provides-foo))
        (provides-foo (make-instance 'test-provides-foo :foo-source 1234)))

    (add-provision provides-foo-class foo)

    (finishes (setf a (make-instance 'action-area)) "make action area")
    (finishes (insert a provides-foo-class) "adding provides-foo-class lives")
    (is (eq (car (gethash 'foo (provision-table a))) provides-foo-class)
        "provides foo is known by the action area to ... provide foo")

    (finishes (insert a provides-foo) "adding instance also ok")
    (is (eq (class-of (car (gethash 'foo (provision-table a))))
            provides-foo-class)
        "now the instance provides foo")))
