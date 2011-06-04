;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8;  -*-
;;;
;;; Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
;;;
;;; See LICENSE for details.

(defpackage #:secure-random
  (:use #:cl)
  (:export #:generator #:*generator* #:bytes #:number)
  (:shadow #:number))

