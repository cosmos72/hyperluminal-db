;; -*- lisp -*-

;; This file is part of hyperluminal-DB.
;; Copyright (c) 2013-2016 Massimiliano Ghilardi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


(in-package :hyperluminal-db-sbcl)

(defconstant +msizeof-slot+ #.(cffi-sys:%foreign-type-size :pointer))

(defconstant +n-address-tag-bits+ (1- (integer-length +msizeof-slot+))
    "Number of low bits that are always ZERO
in a memory address aligned at +MSIZEOF-SLOT+")

(defconstant +n-fixnum-tag-bits+ sb-vm:n-fixnum-tag-bits
  "Number of low bits that are always ZERO
in the representation of a FIXNUM")

;; (assert (>= +n-address-tag-bits+ +n-fixnum-tag-bits+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype maddress () 'fixnum)

(deftype mindex () `(signed-byte ,(- 32 +n-address-tag-bits+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant +defknown-has-overwrite-fndb-silently+
  (dolist (arg (second (sb-kernel:type-specifier (sb-int:info :function :type 'sb-c::%defknown))))
    (when (and (consp arg)
               (eq (first arg) :overwrite-fndb-silently))
      (return t))))

(defmacro defknown (&rest args)
  `(sb-c:defknown ,@args
       ,@(if +defknown-has-overwrite-fndb-silently+ '(:overwrite-fndb-silently t) ())))

