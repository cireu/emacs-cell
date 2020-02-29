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

;; ** Some

;; A Some Cell add the "existence" information for a lisp value.

;; Some Cell can be united with ~nil~ value to present a nullable value.
;; When  writing funtion to query a collection. You can use
;; ~nil~ to present "Not found" and Some(x) to present your found value.
;; When using ~nil~ to present "Not fonud" in tranditional Lisp programming
;; style. The caller of your query function may hard to differentiate
;; these two status:  "Found and get a ~nil~ value" and "Not found".

;; #+begin_src emacs-lisp
;; (defun clear-plist-get (plist key)
;;   (pcase (plist-member plist key)
;;     (`(,_k ,v)
;;       (cell-some v))
;;     (_
;;      nil)))

;; (let ((plist '(:key nil)))
;;   (plist-get plist :key)                ;=> nil
;;   (plist-get plist :val)                ;=> nil

;;   (clear-plist-get plist :key)          ;=> Some(nil)
;;   (clear-plist-get plist :val)          ;=> nil
;;   )
;; #+end_src

;; ~cell.el~ also provides some facility to operates on a Some(x) | nil union.

;; #+begin_src emacs-lisp
;; (let ((some (cell-some 1))
;;       (none nil))
;;   (cell-option-map some #'1+)           ;=> Some(2)
;;   (cell-option-map none #'1+)           ;=> nil

;;   (pcase some
;;     ((cell-some inner)
;;      (message "Some(%d)" inner))
;;     (_ (error "Unreachable!"))))
;; #+end_src

;; ** Box

;; A Box Cell is like a single-element vector, normally used as minimal mutable
;; storage.

;; #+begin_src emacs-lisp
;; (let* ((box (cell-box "Emacs"))
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
;; (let ((weakptr (cell-weak (list 1 2 3 4))))
;;   (cl-assert (equal (cell-weak-get weakptr)
;;                     (cell-option-some (list 1 2 3 4))))
;;   (garbage-collect)
;;   ;; value inside weakptr was GCed because nothing other than a weak cell
;;   ;; held its reference. So `cell-weak-get' will return a None.
;;   (cl-assert (equal (cell-weak-get weakptr)
;;                     (cell-option-none))))
;; #+end_src

;;; Code:

;;; Option

(eval-when-compile (require 'cl-lib))

(cl-defstruct (cell-some
               (:constructor nil)
               (:constructor cell-some--make (-val))
               (:copier nil))
  (-val nil :read-only t))

;; Constructor

;; Make a alias for a meaningful function documentation
(defalias 'cell-some #'cell-some--make
  "Create an option in Some variant with VALUE.
\n(fn VALUE)")

(defun cell-option-from-non-nil (value)
  "Create Some(VALUE) if VALUE if non-nil, otherwise create a None."
  (if value
      (cell-some value)
    nil))

;; Operator

(eval-and-compile
  (pcase-defmacro cell-some (inner)
    "Match a Some(inner) value."
    `(cl-struct cell-some (-val ,inner))))

(defun cell-option-map (option func)
  "Transform the inner of OPTION using FUNC."
  (pcase-exhaustive option
    ((cell-some inner)
     (cell-some (funcall func inner)))
    (`nil nil)))

;;; Box

(cl-defstruct (cell-box
               (:constructor nil)
               (:constructor cell-box--make (-inner))
               (:copier nil))
  -inner)

(defalias 'cell-box 'cell-box--make
  "Create a single mutable unit with INITIAL-VALUE.
\n(fn INITIAL-VALUE)")

(defalias 'cell-box-inner 'cell-box--inner
  "Return the inner value of BOX.
\n(fn BOX)")

;;; Weak

(cl-defstruct (cell-weak
               (:constructor nil)
               (:constructor cell-weak--internal-make)
               (:copier nil))
  (-inner-table (make-hash-table :test #'eq :weakness 'value) :read-only t))

(defun cell-weak (inner)
  "Create a cell takes a weak reference of INNER."
  (if (null inner)
      ;; We could also signal an error, but returning nil
      ;; allow us to treat nil as its own weak-reference,
      nil
    (let* ((cell (cell-weak--internal-make))
           (internal-ht (cell-weak--inner-table cell)))
      (puthash t inner internal-ht)
      cell)))


(defun cell-weak-get (cell)
  "Return inner if reference in CELL still alive, otherwise return nil."
  ;; If nil, it's a weak ref to nil.
  ;; The stored content is never nil, so it only returns nil if
  ;; the content was GC'd.
  (when cell
    (gethash t (cell-weak--inner-table cell))))

(provide 'cell)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cell.el ends here
