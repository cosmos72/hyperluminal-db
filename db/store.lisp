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


(in-package :hyperluminal-db)




(defun get-abi ()
  (load-time-value
   `((:hldb-version      ,+hldb-version+)
     (:hldb-abi-version  ,+hldb-abi-version+)
     (:hlmem-version     ,hlmem::+hlmem-version+)
     (:hlmem-abi-version ,hlmem::+hlmem-abi-version+)
     (:bits-per-byte     ,hlmem::+mem-byte/bits+)
     (:bits-per-tag      ,hlmem::+mem-tag/bits+)
     (:bits-per-vid      ,hlmem::+mem-vid/bits+)
     (:bits-per-int      ,hlmem::+mem-int/bits+)
     (:bits-per-word     ,hlmem::+mem-word/bits+)
     (:bits-per-base-char  ,hlmem::+base-char/bits+)
     (:bits-per-character  ,hlmem::+character/bits+)
     (:sizeof-byte         ,hlmem::+msizeof-byte+)
     (:sizeof-word         ,+msizeof-word+)
     (:sizeof-single-float ,hlmem::+msizeof-sfloat+)
     (:sizeof-double-float ,hlmem::+msizeof-dfloat+)
     (:little-endian       ,hlmem::+mem/little-endian+))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-always
  (defconstant +pagesize+ (os-getpagesize)))


(defvar *fd* +bad-fd+)

(declaim (type mem-size *fwords*))
(defvar *fwords* 0)

;; example MMAP length: 256MB on 32bit archs, 127TB on 64bit archs
(defconstant +max-fwords+
  (let* ;; assume 1/4 of addressable memory can be actually used
      ((arch-max-bytes (ash 1 (- hlmem::+mem-word/bits+ 2)))
       
       ;; assume maximum size of a mmap area is 127TB.
       ;; this is just an example, and not really needed
       (mmap-max-bytes #x7FE000000000)

       ;; compute maximum bytes addressable by a hyperluminal-db:mem-vid
       (persist-max-bytes (* +msizeof-word+ (box-vid->size hlmem::+most-positive-vid+)))
                    
       (max-bytes (min arch-max-bytes mmap-max-bytes persist-max-bytes ))

       ;; round down to a multiple of +pagesize+
       (max-bytes-rounded (logand max-bytes (- +pagesize+))))

    (truncate max-bytes-rounded +msizeof-word+)))


(defvar *p* +null-pointer+)


(defun open-fd (filename &optional (min-words 0))
  (declare (type mem-size min-words))

  (let* ((fd (the fd (os-open-fd-rw filename)))
         (file-words (truncate
                      (the (integer 0) (os-stat-fd-size fd))
                      +msizeof-word+)))
    (unless (zerop min-words)
      (when (< file-words min-words)
        (os-truncate-fd fd (* min-words +msizeof-word+))
        (setf file-words min-words)))

    (the (values fd mem-size)
         (values fd (min +most-positive-size+ file-words)))))


(defun close-fd (fd)
  (declare (type fd fd))
  (os-close-fd fd))


(defun mmap (fd n-words)
  (declare (type fd fd)
           (type mem-size n-words))
  (os-mmap-fd-rw fd 0 (* n-words +msizeof-word+)))

(defun munmap (ptr n-words)
  (declare (type maddress ptr)
           (type mem-size n-words))
  (os-munmap-ptr ptr (* n-words +msizeof-word+)))


(defun msync (ptr n-words &key sync)
  (declare (type maddress ptr)
           (type mem-size n-words)
           (type boolean sync))
  (os-msync-ptr ptr (* n-words +msizeof-word+) sync))


  

(defun init-store (ptr total-n-words)
  "Invoked when loading an unitialized file. Initialize the magic and free-list,
and write them back to file"
  (declare (type maddress ptr)
           (type mem-size total-n-words))
  
  (let* ((index (mfree-head-index (mwrite-magic ptr 0 total-n-words)))
         (free-list (init-free-list ptr index total-n-words)))
    (mwrite-free-list ptr free-list)
    (msync ptr total-n-words)
    free-list))




(defun hldb-open (&key (filename "mmap") (min-words #.(truncate +pagesize+ +msizeof-word+)))
  (declare (type mem-size min-words))

  ;; open file and (if needed) extend it
  (multiple-value-bind (fd words) (open-fd filename min-words)
    (setf *fd* fd
          *fwords* words)
                                        
    (let ((ptr))

      (unwind-protect
           (progn
             (setf ptr (mmap fd words)
                   *p* ptr)
             (if-bind index (mread-magic ptr 0 words)
                 (let1 index (mfree-head-index index)
                   (mread-free-list ptr index))
                 (init-store ptr words)))

        (unless ptr
          (close-fd *fd*)
          (setf *fd* +bad-fd+))))))
    

         



(defun hldb-close ()
  (unless (null-pointer? *p*)
    (munmap *p* *fwords*)
    (setf *p* +null-pointer+))
  (setf *mfree* nil)
  (unless (eql +bad-fd+ *fd*)
    (close-fd *fd*)
    (setf *fd* +bad-fd+
          *fwords* 0)))




