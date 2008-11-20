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

;; setup stuff for example
(defclass ssh-server ()
  ((computer-name :initarg :very-poorly-named-initarg
                  :accessor computer-name)
   (some-other-arg :initarg :some-other-arg
                   :accessor some-other-arg))
  (:metaclass with-provides-requires))

(define-requirement computer-name)
(add-requirement (find-class 'ssh-server) "computer-name" computer-name)

(test basic-instance-creation
  "create instances from stuff in the action area"

  (let ((area (make-instance 'action-area)) instance)
    (insert area (cons computer-name "My Computer"))

    ;; try the normal way of using make-instance
    (finishes (setf instance
                    (make-instance 'ssh-server
                                   :very-poorly-named-initarg "foo"
                                   :some-other-arg "bar")))
    (is (string= (computer-name instance) "foo"))
    (is (string= (some-other-arg instance) "bar"))

    ;; test the initarg builder
    (is (endp
         (build-initargs-for (find-class 'ssh-server) area
                             '(:very-poorly-named-initarg :some-other-arg)))
        "if we ignore all initargs, return nothing")

    (is (equal (build-initargs-for (find-class 'ssh-server) area
                                   '(:some-other-arg))
               (list :very-poorly-named-initarg "My Computer")))

    ;; then try with an action-area
    (finishes (setf instance
                    (make-instance 'ssh-server
                                   :from-action-area area
                                   :some-other-arg "oh hai")))
    (is (string= (computer-name instance) "My Computer"))
    (is (string= (some-other-arg instance) "oh hai"))

    ;; pass an action-area, but don't actually use it
    (finishes (setf instance
                    (make-instance 'ssh-server
                                   :from-action-area area
                                   :very-poorly-named-initarg "not my computer"
                                   :some-other-arg "oh hello")))
    (is (string= (computer-name instance) "not my computer"))
    (is (string= (some-other-arg instance) "oh hello"))))
