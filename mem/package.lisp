;; -*- lisp -*-

;; This file is part of hyperluminal-DB.
;; Copyright (c) 2013 Massimiliano Ghilardi
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


;;;; * HYPERLUMINAL-DB

(in-package :cl-user)

(defpackage #:hyperluminal-mem

  (:nicknames #:hl-mem)

  (:use #:cl)

  (:import-from #:stmx.lang

                #:eval-always  #:enable-#?-syntax

                #:set-feature  #:set-features #:default-feature #:default-features
                #:get-feature  #:all-features?

                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen    #:aif 
                #:log.debug    #:log.trace     #:log.make-logger)

  (:import-from #:stmx
                #:+unbound-tvar+)

  (:import-from #:stmx.util
                
                #:fixnum< #:fixnum> #:fixnum=

                #:_ #:tcell #:tcons #:tlist #:tstack #:tfifo

                #:tmap #:rbmap #:gmap #:gmap-pred #:gmap-count #:set-gmap #:do-gmap

                #:thash-table #:ghash-table #:ghash-table-test #:ghash-table-hash
                #:ghash-table-count #:set-ghash #:do-ghash)
                

  (:export #:malloc #:malloc-words #:mfree #:with-mem-words #:+msizeof-word+
           #:msize        #:mwrite         #:mread
           #:msize-object #:mwrite-object  #:mread-object
           #:mget-unboxed #:mset-unboxed
           #:mread-magic  #:mwrite-magic))