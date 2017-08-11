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

(defmacro do-axes-hashed (axes function char &body body)
  `(,function
     ,@(loop for i from 0 upto (1- (length (second (first axes))))
          collect `(progn
                     ,@(operate-sexp body char #'operate-symbol-hashed (list axes i))))))

(defun operate-symbol-hashed (symbol original-symbol char axes index)
  (multiple-value-bind (interval start end)
      (find-interval symbol original-symbol char)
    (if (null interval)
        symbol
        (let ((key (find interval axes :key #'first-name :test #'equalp)))
          (unless key
            (error "Key ~A not found in axes ~A" interval axes))
          (operate-symbol-hashed
           (replace-section symbol
                            (nth index (second key))
                            start end)
           original-symbol
           char
           axes
           index)))))
