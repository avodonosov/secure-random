;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(asdf:defsystem #:secure-random
  :description "Cryptographically secure pseudo-random number generator for Common Lisp"
  :author "Anton Vodonosov <avodonosov@yandex.ru>"
  :license "MIT"
  :serial t
  :version "0.1.0"
  :depends-on (#:cl+ssl)
  :components ((:file "package")
               (:file "secure-random")))

