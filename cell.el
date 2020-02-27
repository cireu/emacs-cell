;;; cell.el --- Value with extra information -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhu Zihao

;; Author: Zhu Zihao <all_but_last@163.com>
;; URL: https://github.com/cireu/emacs-cell
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; * Introduction

;; This library provides a set of "Cell" for Elisp programming. A "Cell" is a
;; container of single Lisp value, with extra information attached on it.

;; Currently there are 3 kinds of "Cell" in ~cell.el~

;; ** Option

;; A Option Cell add the "existence" information for a lisp value.
;; A Option Cell may in 2 variant, ~Some~ variant means there's a value and
;; ~None~ variant means there's no value.

;; Option Cell is useful when writing funtion to query a collection. You can use
;; ~None~ variant to present "Not found". When using ~nil~ to present "Not
;;    fonud" in tranditional Lisp programming style. The caller of your query
;; function may have mistaken on "Found and get a ~nil~ value" and "Not found".

;; #+begin_src emacs-lisp
;; (defun clear-plist-get (plist key)
;;   (pcase (plist-member plist key)
;;     (`(,_k ,v)
;;       (cell-option-some v))
;;     (_
;;      (cell-option-none))))

;; (let ((plist '(:key nil)))
;;   (plist-get plist :key)                ;=> nil
;;   (plist-get plist :val)                ;=> nil

;;   (clear-plist-get plist :key)          ;=> Some(nil)
;;   (clear-plist-get plist :val)          ;=> None
;;   )
;; #+end_src

;; ~cell.el~ also provides some facility to operates on a Option Cell.

;; #+begin_src emacs-lisp
;; (let ((some (cell-option-some 1))
;;       (none (cell-option-none)))
;;   (cell-option-map some #'1+)           ;=> Some(2)
;;   (cell-option-map none #'1+)           ;=> None

;;   (cell-option-if-let (inner some)
;;       (message "Some(%d)" inner)
;;     (error "Unreachable!")))
;; #+end_src

;; ** Box

;; A Box Cell is like a single-element vector, normally used as minimal mutable
;; storage.

;; #+begin_src emacs-lisp
;; (let* ((box (cell-box-make "Emacs"))
;;        (lst (list box))
;;        (vec (vector box)))
;;   (cl-assert (eq (cell-box-inner (nth 0 lst))
;;                  (cell-box-inner (aref vec 0))))

;;   (setf (cell-box-inner (nth 0 lst)) "emacs")
;;   (cl-assert (eq (cell-box-inner (nth 0 lst))
;;                  (cell-box-inner (aref vec 0)))))
;; #+end_src


;; ** Weak

;; A Weak Cell is like weak pointer in other languages. The reference in Weak
;; Cell will not be considered in GC. This can help you manage struct with
;; cyclic reference.

;; When design a RAII style API in Emacs Lisp, Weak Cell can help you
;; differentiate "first class" reference and "second class" reference. And
;; make sure finalizer can be run as expected.

;; #+begin_src emacs-lisp
;; (let ((weakptr (cell-weak-make (list 1 2 3 4))))
;;   (cl-assert (equal (cell-weak-get weakptr)
;;                     (cell-option-some (list 1 2 3 4))))
;;   (garbage-collect)
;;   ;; value inside weakptr was GCed because nothing other than a weak cell
;;   ;; held its reference. So `cell-weak-get' will return a None.
;;   (cl-assert (equal (cell-weak-get weakptr)
;;                     (cell-option-none))))
;; #+end_src

;; * Contributions

;; Please report an issue or PR if you encounter any problem when using the library.

;;; Code:

;;; Option

(eval-when-compile (require 'cl-lib))

(cl-defstruct (cell-option
               (:constructor nil)
               (:constructor cell-option--internal-make (-val))
               (:copier nil))
  (-val nil :read-only t))

;; Constructor

(defsubst cell-option-some (value)
  "Create an option in Some variant with VALUE."
  (cell-option--internal-make value))

(defconst cell-option--none-singleton
  (let ((sym (make-symbol "rstream--none")))
    (cell-option--internal-make sym)))

(defsubst cell-option-none ()
  "Create an option in None variant."
  cell-option--none-singleton)

(defun cell-option-from-non-nil (value)
  "Create Some(VALUE) if VALUE if non-nil, otherwise create a None."
  (if value
      (cell-option-some value)
    (cell-option-none)))

;; Predicator

(defun cell-option-none-p (option)
  "Return non-nil if OPTION in None variant."
  (if (cl-typep option 'cell-option)
      (eq option (cell-option-none))
    (signal 'wrong-type-argument
            (list 'cell-option option 'option))))

(defsubst cell-option-some-p (option)
  "Return non-nil if OPTION in Some variant."
  (not (cell-option-none-p option)))

;; Operator

(cl-defmacro cell-option-if-let ((var val) then &rest else)
  "Bind the inner of VAL to VAR and yield THEN if VAL is a Some, or yield ELSE."
  (declare (indent 2) (debug ((symbolp form) form body)))
  (macroexp-let2 nil val val
    `(if (cell-option-some-p ,val)
         (let ((,var (cell-option--val ,val)))
           ,then)
       ,@else)))

(cl-defmacro cell-option-when-let ((var val) &rest forms)
  "Bind the inner of VAL to VAR and yield FORMS if VAL is a Some."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(cell-option-if-let (,var ,val)
       ,(macroexp-progn forms)))

(defun cell-option-map (option func)
  "Transform the inner of OPTION using FUNC."
  (cell-option-if-let (inner option)
      (cell-option-some (funcall func inner))
    option))

(defalias 'cell-option-inner-unchecked 'cell-option--val
  "Return the inner value of OPTION directly, skip status checking.

It's your guarantee to ensure the OPTION is a Some to get meaningful result.
\n(fn OPTION)")

;;; Box

(cl-defstruct (cell-box
               (:constructor nil)
               (:constructor cell-box--make (-inner))
               (:copier nil))
  -inner)

(defalias 'cell-box-make 'cell-box--make
  "Create a single mutable unit with INITIAL-VALUE.")

(defalias 'cell-box-inner 'cell-box--inner
  "Return the inner value of BOX.
\n(fn BOX)")

;;; Weak

(cl-defstruct (cell-weak
               (:constructor nil)
               (:constructor cell-weak--internal-make)
               (:copier nil))
  (-inner-table (make-hash-table :test #'eq :weakness 'value) :read-only t))

(defun cell-weak-make (inner)
  "Create a cell takes a weak reference of INNER."
  (let* ((cell (cell-weak--internal-make))
         (internal-ht (cell-weak--inner-table cell)))
    (puthash t inner internal-ht)
    cell))

(defun cell-weak-get (cell)
  "Return Some(inner) if reference in CELL still alive, otherwise return None."
  (let* ((gensym (make-symbol "not-found"))
         (val (gethash t (cell-weak--inner-table cell) gensym))
         (found (not (eq gensym val))))
    (if found
        (cell-option-some val)
      (cell-option-none))))

(provide 'cell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cell.el ends here
