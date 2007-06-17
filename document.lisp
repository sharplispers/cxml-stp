;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cxml-stp)

#+sbcl
(declaim (optimize (debug 2)))


;;;; Class DOCUMENT

(defun make-document (document-element)
  (check-type document-element element)
  (let ((result (make-instance 'document)))
    (insert-child result document-element 0)
    result))

(defmethod copy ((node document))
  (let ((result (make-instance 'document)))
    (insert-child result (copy (document-element node)) 0)
    ;; zzz das ist doch nicht schoen so
    (let ((i 0))
      (do-children (child node)
	(unless (typep child 'element)
	  (insert-child result i (copy child)))
	(incf i)))
    (setf (%base-uri result) (%base-uri node))
    result))

(defun assert-orphan (node)
  (when (parent node)
    (stp-error "node already has a parent: ~A" node)))

(defmethod check-insertion-allowed ((parent document) child i)
  (assert-orphan child)
  (typecase child
    ((or comment processing-instruction))
    (document-type
     (when (document-type parent)
       (stp-error "attempt to insert multiple document types"))
     (let ((j (child-position-if (alexandria:of-type 'element) parent)))
       (unless (<= i j)
	 (stp-error
	  "attempt to insert document type after document element"))))
    (element
     (unless (alexandria:emptyp (%children parent))
       (stp-error "attempt to insert multiple document elements")))
    (t
     (stp-error "not a valid child of a document: ~A" child))))

(defmethod check-deletion-allowed ((parent document) (child node) i)
  nil)
(defmethod check-deletion-allowed ((parent document) (child element) i)
  (stp-error "attempt to remove document element"))

(defmethod check-replacement-allowed ((parent document) children)
  (unless children
    (stp-error "attempt to remove document element"))
  (let ((dt nil)
	(de nil))
    (loop
       for i from 0
       for c across children
       do
	 (typecase c
	   ((or comment processing-instruction))
	   (element
	    (when de
	      (stp-error "attempt to insert multiple document elements"))
	    (setf de i))
	   (document-type
	    (when dt
	      (stp-error "attempt to insert multiple document types"))
	    (setf dt i))
	   (t
	    (stp-error "not a valid child of a document: ~A" c))))
    (when (and dt (> dt de))
      (stp-error "attempt to insert document type after document element"))))

(defun document-type (document)
  (find-if (alexandria:of-type 'document-type) (%children document)))

;; zzz gefaellt mir nicht
(defun (setf document-type) (newval document)
  (check-type newval document-type)
  (let ((old (document-type document)))
    (unless (eq newval old)
      (assert-orphan newval)
      (if old
	  (let ((pos (position old (%children document))))
	    (delete-nth-child pos document)
	    (insert-child document newval pos))
	  (insert-child document newval 0)))))

(defun document-element (document)
  (find-if (alexandria:of-type 'element) (%children document)))

;; zzz gefaellt mir nicht
(defun (setf document-element) (newval document)
  (check-type newval element)
  (let ((old (document-element document)))
    (unless (eq newval old)
      (assert-orphan newval)
      (let ((pos (position old (%children document))))
	(%nuke-nth-child document pos)
	(insert-child document newval pos)))))

(defmethod base-uri ((document document))
  (%base-uri document))

(defmethod (setf base-uri) (newval (document document))
  (setf (%base-uri document) newval))

(defmethod string-value ((node document))
  (string-value (document-element node)))

(defmethod unparse ((node document) handler)
  (sax:start-document handler)
  (map nil (lambda (x) (unparse x handler)) (%children node))
  (sax:end-document handler))

(defreader document ())
