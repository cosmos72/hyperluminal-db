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


(defun deref (address offset)
  (declare (type addr address)
           (type (or fixnum addr-offset) offset))
  (deref address offset))


(declaim (inline set-deref))

(defun set-deref (address offset value)
  (declare (type addr address)
           (type (or fixnum addr-offset) offset)
           (type t value))
  (%set-deref address offset value)
  value)

(defsetf deref set-deref)
