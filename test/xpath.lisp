(in-package :cxml-stp-test)

(defun run-xpath-tests ()
  (let ((xpath-test:*dom-builder* (stp:make-builder))
        (xpath-test:*document-element* #'stp:document-element))
    (xpath-test::run-all-tests)))
