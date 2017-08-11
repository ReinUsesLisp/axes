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

(defmacro do-axes-individual (axes function char &body body)
  (check-individual-axes axes)
  `(,function
     ,@(loop for axis in axes
          collect `(progn
                     ,@(operate-sexp body char #'operate-symbol-individual (list axis))))))

(defun operate-symbol-individual (symbol original-symbol char replace)
  (let* ((name (symbol-name symbol))
         (start (position char name)))
    (if start
        (operate-symbol-individual
         (replace-section symbol replace start)
         original-symbol
         char
         replace)
        symbol)))

(defun check-individual-axes (axes)
  (declare (ignore axes)))
