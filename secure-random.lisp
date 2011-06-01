;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(in-package #:secure-random)

;; ************* The libray public interface **************

(defclass state () ()
  (:documentation "The base class for all the possible implementations of 
secure random number generator state."))

(defvar *state*
  "Current value of the random number generator state. Used as the 
default value for the library functions parameter STATE.")

(defgeneric bytes (count state)
  (:documentation "The only generic function which needs to be implemented by a subclass
of SECURE-RANDOM:STATE. Generates COUNT cryptographically strong pseudo-random 
bytes using the random number generator STATE. Returns the bytes as a 
SIMPLE-ARRAY with ELEMENT-TYPE '(UNSIGNED-BYTE 8). Signals
an ERROR in case of problems (for example when the random number
generator has not been initialized with enough entrophy)."))

(defun number (limit &optional (state *state*))
  "Returns a cryptographically strong pseudo-random number that is a 
non-negative number less than LIMIT and of the same type as LIMIT 
(in the current implementation, only INTEGER type is supporeted).
LIMIT is a positive number. STATE is an instance of a 
subclass of the SECURE-RANDOM:STATE. Signals an ERROR in case 
of problems (for example when the random number generator has not been 
initialized with enough entrophy)."
  (let ((bytes-needed (1+ (truncate (/ (log limit 2)
                                       8)))))
    (mod (octets-to-integer (bytes bytes-needed state))
         limit)))

;; ***************** Utils *********************

(defun octets-to-integer (octets-vec)
  (declare (type (array (unsigned-byte 8) (*)) octets-vec))
  (loop with sum = 0 
     for octet across octets-vec
     do (setf sum (+ octet (ash sum 8)))
     finally (return sum)))

;; ********* The implementation of the public interface ********

;; The STATE implementation which uses OpenSSL 
;; random number generator
(defclass open-ssl-state (state) ())

(defmethod bytes (count (state open-ssl-state))
  (cl+ssl:random-bytes count))

;; Use the OpenSSL RNG as the default implementation
(setf *state* (make-instance 'open-ssl-state))
