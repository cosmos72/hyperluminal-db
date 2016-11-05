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

(declaim (inline deref))

(defknown deref
    ;;arg-types
    (addr (or fixnum mindex))
    ;;result-type
    t
    (sb-c::flushable sb-c::important-result sb-c::always-translatable))

(sb-c:define-vop (deref)
  (:policy :fast-safe)
  (:translate deref)

  ;; directly use a tagged FIXNUM as address... on SBCL
  ;; its representation is shifted by +n-fixnum-tag-bits+
  ;; which means that only addresses aligned at 2 or 4 bytes can be represented
  (:args (address :scs (sb-vm::any-reg))
         (index   :scs (sb-vm::any-reg)))
  (:arg-types sb-vm::tagged-num
              sb-vm::tagged-num)
  (:results   (r :scs (sb-vm::descriptor-reg)))
  (:result-types *)

  (:generator 5
   (sb-assem:inst mov r
                  (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                  :base address
                                  :index index
                                  :scale (ash 1 (- +n-address-tag-bits+ +n-fixnum-tag-bits+))))))

(sb-c:define-vop (deref-c)
  (:policy :fast-safe)
  (:translate deref)

  ;; directly use a tagged FIXNUM as address... on SBCL
  ;; its representation is shifted by +n-fixnum-tag-bits+
  ;; which means that only addresses aligned at 2 or 4 bytes can be represented
  (:args (address :scs (sb-vm::any-reg)))
  (:info index)
  (:arg-types sb-vm::tagged-num
              (:constant mindex))
  (:results   (r :scs (sb-vm::descriptor-reg)))
  (:result-types *)

  (:generator 4
   (sb-assem:inst mov r
                  (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                  :base address
                                  :disp (the (signed-byte 32) (ash index +n-address-tag-bits+))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline %set-deref))

(defknown set-deref
    ;;arg-types
    (addr (signed-byte 32) t)
    ;;result-type
    (values)
    (sb-c::always-translatable))


(sb-c:define-vop (%set-deref)
  (:policy :fast-safe)
  (:translate %set-deref)

  ;; directly use a tagged FIXNUM as address... on SBCL
  ;; its representation is shifted by +n-fixnum-tag-bits+
  ;; which means that only addresses aligned at 2 or 4 bytes can be represented
  (:args (address :scs (sb-vm::any-reg))
         (index   :scs (sb-vm::any-reg))
         (value   :scs (sb-vm::descriptor-reg)))
  (:arg-types sb-vm::tagged-num
              sb-vm::tagged-num
              *)
  (:generator 5
   (sb-assem:inst mov
                  (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                  :base address
                                  :index index
                                  :scale (ash 1 (- +n-address-tag-bits+ +n-fixnum-tag-bits+)))
                  value)))


(sb-c:define-vop (%set-deref-c)
  (:policy :fast-safe)
  (:translate %set-deref)

  ;; directly use a tagged FIXNUM as address... on SBCL
  ;; its representation is shifted by +n-fixnum-tag-bits+
  ;; which means that only addresses aligned at 2 or 4 bytes can be represented
  (:args (address :scs (sb-vm::any-reg))
         (value   :scs (sb-vm::descriptor-reg)))
  (:info index)
  (:arg-types sb-vm::tagged-num
              (:constant mindex)
              *)
  (:generator 5
   (sb-assem:inst mov
                  (sb-vm::make-ea #+x86 :dword #-x86 :qword
                                  :base address
                                  :disp (ash index +n-address-tag-bits+))
                  value)))

