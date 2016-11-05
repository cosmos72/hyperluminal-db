;; -*- lisp -*-

;; This file is part of Hyperluminal-db.
;; Copyright (c) 2016 Massimiliano Ghilardi
;;

;;;; * HYPERLUMINAL-DB-SBCL

(in-package :cl-user)


(defpackage #:hyperluminal-db-sbcl
  (:nicknames #:hldb-sbcl)
  (:use #:cl)

  (:export #:addr  #:sap=>addr  #:addr=>sap  #:addr-ref-obj))
