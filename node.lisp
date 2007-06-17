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

(defvar *check-uri-syntax* t)
(defun check-namespace-uri (uri)
  (when (and *check-uri-syntax* (not (search "://" uri)))
    (warn "namespace URI does not look like an absolute URL: ~S" uri)))

(define-condition rng-error (simple-error)
  ()
  (:documentation "The class of all STP errors."))

(defun stp-error (fmt &rest args)
  "@unexport{}"
  (error 'rng-error :format-control fmt :format-arguments args))


;;;; Class NODE

(defgeneric string-value (node))

(defun document (node)
  (check-type node node)
  (loop
     for parent = node then (parent parent)
     while (and parent (not (typep parent 'document)))
     finally (return parent)))

(defun root (node)
  (check-type node node)
  (loop
     for p = (parent node) then (parent p)
     and q = node then p
     while p
     finally (return q)))

(defgeneric base-uri (node)) ;fixme: hier muessen wir wissen, ob specified
(defmethod base-uri ((node node))
  (let ((parent (parent node)))
    (if parent
        (base-uri parent)
        "")))

(defgeneric detach (node))
(defmethod detach ((node node))
  (when (parent node)
    (delete-child node (parent node))))

;;; kinderkram
;;; das ist noch unvollstaendig
(defgeneric map-children (result-type function node))
(defmacro do-children ((var node &optional result) &body body)
  `(block nil
     (map-children nil (lambda (,var) ,@body) ,node)
     ,result))
(defun list-children (node)
  (map-children 'list #'identity node))

(defgeneric copy (node))
(defgeneric unparse (node handler))

;; print-object nicht vergessen

;;; (defun query (node xpath)
;;;   ;; fixme
;;;   )

(defgeneric slots-for-print-object (node)
  (:method-combination append))

(defmethod slots-for-print-object append ((node parent-node))
  '((:base-uri %base-uri)
    (:children list-children)))

(defmethod print-object ((object node) stream)
  (when (and *print-readably* (not *read-eval*))
    (error "cannot print STP nodes readably without *read-eval*"))
  (if *print-pretty*
      (pretty-print-node object stream)
      (ugly-print-node object stream)))

(defun pretty-print-node (node stream)
  (let* ((slots (slots-for-print-object node))
	 (constructor (class-name (class-of node)))
	 (level *print-level*)
	 (length *print-length*)
	 (*print-level* nil)
	 (*print-length* nil))
    (pprint-logical-block (stream nil :prefix "#.(" :suffix ")")
      (write constructor :stream stream)
      (when (parent node)
	(write-char #\space stream)
	(pprint-newline :linear stream)
	(pprint-pop)
	(format stream "#| ~S of type ~A |#"
		:parent
		(type-of (parent node))))
      (let ((remaining-slots slots))
        (when remaining-slots
          (write-char #\space stream)
          (pprint-newline :linear stream)
          (loop
	     (pprint-pop)
	     (destructuring-bind (key fn) (pop remaining-slots)
	       (write key :stream stream)
	       (write-char #\space stream)
	       (pprint-newline :miser stream)
	       (let ((value (funcall fn node))
		     (*print-level* level)
		     (*print-length* length))
		 (unless (typep value '(or string null))
		   (write-char #\' stream))
		 (write value :stream stream))
	       (when (null remaining-slots)
		 (return))
	       (write-char #\space stream)
	       (pprint-newline :linear stream))))))))

(defun ugly-print-node (node stream)
  (let* ((slots (slots-for-print-object node))
	 (constructor (class-name (class-of node)))
	 (level *print-level*)
	 (length *print-length*)
	 (*print-level* nil)
	 (*print-length* nil))
    (write-string "#.(" stream)
    (write constructor :stream stream)
    (let ((remaining-slots slots))
      (when remaining-slots
	(write-char #\space stream)
	(loop
	   (destructuring-bind (key fn) (pop remaining-slots)
	     (write key :stream stream)
	     (write-char #\space stream)
	     (let ((value (funcall fn node))
		   (*print-level* level)
		   (*print-length* length))
	       (unless (typep value '(or string null))
		 (write-char #\' stream))
	       (write value :stream stream))
	     (when (null remaining-slots)
	       (return))
	     (write-char #\space stream)))))
    (write-string ")" stream)))

(defgeneric reconstruct (node &key &allow-other-keys)
  (:method-combination progn))

(defmacro defreader (name (&rest args) &body body)
  `(progn
     (defun ,name (&rest keys)
       "@unexport{}"
       (let ((result (make-instance ',name)))
	 (apply #'reconstruct result keys)
	 result))
     (defmethod reconstruct
	 progn
	 ((this ,name)
	  &key ,@(loop
		    for arg in args
		    collect `(,arg (error "slot ~A missing in printed representation"
					  ',arg)))
	  &allow-other-keys)
       ,@body)))
