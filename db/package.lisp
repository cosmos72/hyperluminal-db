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

(defpackage #:hyperluminal-db

  (:nicknames #:hl-db #:hldb)

  (:use #:cl)

  (:import-from #:stmx.lang

                #:enable-#?-syntax  #:eval-always  #:set-feature 

                #:define-global                #:define-constant-once
                #:with-gensym  #:with-gensyms  #:new      #:let1
                #:when-bind    #:if-bind       #:awhen    #:aif 
                #:log.debug    #:log.trace     #:log.make-logger)

  (:import-from #:hyperluminal-mem-ffi

                #:+null-pointer+ #:null-pointer?
                
                #:os-getpagesize #:+bad-fd+
                
                #:os-open-fd #:os-close-fd   #:os-stat-fd-size #:os-truncate-fd 
                #:os-mmap-fd #:os-munmap-ptr #:os-msync-ptr)
                
  (:import-from #:hyperluminal-mem

                #:+null-pointer+         #:+mem-unallocated+
                #:+most-positive-size+   #:+msizeof-word+
                #:+mem-box/min-words+    #:+mem-box/max-words+
                #:+mem-box/header-words+ #:+mem-box/min-payload-words+

                #:maddress   #:mem-word
                #:msizeof    #:mget-t    #:mset-t
                
                #:mzero-words
                
                #:mem-size #:mem-size+ #:mem-size+1 #:mem-size- #:mem-size-1
                #:incf-mem-size        #:mem-size*

                #:mem-tag  #:mem-vid   #:mget-vid
                #:with-tag-and-vid     #:mset-tag-and-vid

                #:mdetect-box-type     #:mread-box  #:msize-box-rounded-up
                #:mwrite-box/header    #:mwrite-box
                #:box-vid->size #:size->box-vid

                #:mread-magic #:mwrite-magic)
                                


  (:export      #:hldb-version #:hldb-abi-version
                #:hldb-open    #:hldb-close))
                
