(cl:defpackage #:cxml-stp-system
  (:use #:asdf #:cl))
(cl:in-package #:cxml-stp-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem "cxml-stp"
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "classes")
     (:file "node")
     (:file "parent-node")
     (:file "leaf-node")
     (:file "document")
     (:file "element")
     (:file "attribute")
     (:file "document-type")
     (:file "comment")
     (:file "processing-instruction")
     (:file "text")
     (:file "builder")
     (:file "xpath"))
    :depends-on ("cxml" "alexandria" "xpath")
    :in-order-to ((test-op (test-op "cxml-stp/test"))))

(defsystem "cxml-stp/test"
    :default-component-class closure-source-file
    :serial t
    :components ((:module "test"
                  :components ((:file "test")
                               (:file "xpath"))))
    :depends-on ("cxml-stp" "rt" "xpath/test")
    :perform (test-op (operation component)
               (uiop:symbol-call '#:cxml-stp-test '#:do-tests)
               (uiop:symbol-call '#:cxml-stp-test '#:run-xpath-tests)))
