;; -*- lisp -*-

;; This file is part of HYPERLUMINAL-DB.
;; Copyright (c) 2013-2015 Massimiliano Ghilardi
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



(in-package :cl-user)

(asdf:defsystem :hyperluminal-db
  :name "HYPERLUMINAL-DB"
  :version "0.5.1"
  :license "GPLv3"
  :author "Massimiliano Ghilardi"
  :description "Persistent, transactional object store."

  :depends-on (:log4cl
               :stmx
               :hyperluminal-mem
               :trivial-garbage)

  :components
  ((:static-file "hyperluminal-db.asd")
	       
   (:module :db
    :components ((:file "package")
		 (:file "version"        :depends-on ("package"))
		 (:file "ffi-btree"      :depends-on ("version"))
		 (:file "box"            :depends-on ("version"))
		 (:file "alloc"          :depends-on ("box"))
		 (:file "store"          :depends-on ("alloc"))))))


(asdf:defsystem :hyperluminal-db-test
  :name "HYPERLUMINAL-DB-TEST"
  :version "0.5.1"
  :author "Massimiliano Ghilardi"
  :license "GPLv3"
  :description "test suite for hyperluminal-db"

  :depends-on (:log4cl
               :fiveam
               :hyperluminal-db)

  :components ((:module :test
                :components ((:file "package")))))



(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :hyperluminal-db))))
  (ql:quickload "hyperluminal-db-test")
  (eval (read-from-string "(fiveam:run! 'hyperluminal-db-test:suite)")))
