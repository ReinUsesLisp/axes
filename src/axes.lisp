;;;;
;;;;    This file is part of axes.
;;;;
;;;;    axes is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Lesser General Public License as
;;;;    published by the Free Software Foundation, either version 3 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    axes is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public License
;;;;    along with axes.  If not, see <http://www.gnu.org/licenses/>.
;;;;

(in-package :axes)

(defmacro progn-axes (axes &body body)
  (check-axes axes)
  `(progn
     ,@(loop for axis in axes
          collect `(progn ,@(operate-sexp body axis)))
     (values)))

(defmacro do-axes (axes &body body)
  `(progn-axes ,axes ,@body))

(defmacro map-axes (axes &body body)
  (check-axes axes)
  (let ((result (gensym "RESULT")))
    `(let ((,result nil))
       ,@(loop for axis in axes
            collect `(push (progn ,@(operate-sexp body axis)) ,result))
       (nreverse ,result))))

(defun operate-sexp (sexp axis)
  (cond
    ((listp sexp)
     (if (member (first sexp) '(progn-axes do-axes map-axes))
         sexp                           ; guard nested expansion
         (loop for sub-sexp in sexp
            collect (operate-sexp sub-sexp axis))))
    ((symbolp sexp) (operate-symbol sexp axis))
    (t sexp)))

(defun operate-symbol (symbol axis)
  (if (symbolp axis)
      (operate-symbol-individual symbol axis)
      (operate-symbol-multiple symbol axis)))

(defun operate-symbol-individual (symbol replace)
  (let* ((name (symbol-name symbol))
         (start (position #\@ name)))
    (if start
        (operate-symbol-individual
         (replace-section symbol replace start)
         replace)
        symbol)))

(defun operate-symbol-multiple (symbol axis &optional (original-symbol symbol))
  (declare (symbol symbol original-symbol) (list axis))
  (let* ((name (symbol-name symbol))
         (start (position #\@ name)))
    (if start
        (let ((delta (position #\@ (subseq name (1+ start)))))
          ;; check if there is a closing @
          (unless delta
            (error "No @N@ interval defined in symbol ~A." original-symbol))
          (let* ((end (+ start delta 1))
                 (index (parse-integer (subseq name (1+ start) end))))
            ;; check if N is not out of scope
            (unless (< -1 (1- index) (length axis))
              (error "Invalid index ~D in symbol ~A." index original-symbol))
            (operate-symbol-multiple
             (replace-section symbol (nth (1- index) axis) start end)
             axis
             original-symbol)))
        symbol)))

(defun replace-section (symbol replace start &optional (end start))
  (let ((name (symbol-name symbol)))
    (intern (format nil "~A~A~A"
                    (subseq name 0 start)
                    replace
                    (subseq name (1+ end)))
            (symbol-package symbol))))

(defun check-axes (axes)
  (when (and (some #'listp axes) (some #'symbolp axes))
    (error "You can't mix multiple axes with individual axes. ~A" axes))
  (when (and (every #'listp axes) (reduce #'/= axes :key #'length))
    (error "You can't mix axes different sizes. ~A" axes)))
