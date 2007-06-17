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


;;;; Class PROCESSING-INSTRUCTION

(defclass processing-instruction (leaf-node)
  ((target :initarg :target :accessor target)
   (data :initarg :data :accessor data)))

(defun make-processing-instruction (target data)
  (let ((result (make-instance 'processing-instruction)))
    (setf (target processing-instruction) target)
    (setf (data processing-instruction) data)
    result))

(defmethod copy ((node processing-instruction))
  (make-instance 'processing-instruction
    :target (target processing-instruction)
    :data (data processing-instruction)))

(defmethod string-value ((node attribute))
  (data node))

(defmethod (setf target) :before (newval (node processing-instruction))
  (check-nc-name newval)
  (unless (string-equal newval "xml")
    (stp-error "attempt to pretend that a PI is an XMLDecl")))

(defmethod (setf data) :before (newval (node processing-instruction))
  (unless newval (setf newval ""))
  (unless (xml-characters-p newval)
    (stp-error "Processing instruction data includes characters that ~
                cannot be represented in XML at all: ~S"
	       newval))
  (when (search "?>" newval)
    (stp-error "forbidden -- in processing-instruction"))
  (when (or (alexandria:starts-with 10 newval :key #'char-code)
	    (alexandria:starts-with 13 newval :key #'char-code)
	    (alexandria:starts-with 32 newval :key #'char-code))
    (stp-error "space at beginning of processing instruction data")))

(defmethod unparse ((node processing-instruction) handler)
  (sax:processing-instruction handler (target node) (data node)))