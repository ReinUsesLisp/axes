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

(defmacro do-axes-multiple (axes function char &body body)
  (check-multiple-axes axes)
  `(,function
     ,@(loop for axis in axes
          collect `(progn
                     ,@(operate-sexp body char #'operate-symbol-multiple (list axis))))))

(defun operate-symbol-multiple (symbol original-symbol char axis)
  (declare (symbol symbol original-symbol) (list axis))
  (multiple-value-bind (interval start end)
      (find-interval symbol original-symbol char)
    (if (null interval)
        symbol
        (let ((index (parse-integer interval)))
          ;; check if index is not out of scope
          (unless (< -1 (1- index) (length axis))
            (error "Invalid index ~D in symbol ~A." index original-symbol))
          (operate-symbol-multiple
           (replace-section symbol (nth (1- index) axis) start end)
           original-symbol
           char
           axis)))))

(defun check-multiple-axes (axes)
  (when (reduce #'/= axes :key #'length)
    (error "Mixed sizes in axes ~A" axes)))
