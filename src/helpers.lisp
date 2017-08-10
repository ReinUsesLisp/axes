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

(defun find-interval (symbol original-symbol)
  (let* ((name (symbol-name symbol))
         (start (position #\@ name)))
    (if (null start)
        (values nil nil nil)
        (let ((delta (position #\@ (subseq name (1+ start)))))
          ;; check if there is a closing @
          (unless delta
            (error "No @-@ interval defined in symbol ~A." original-symbol))
          (let* ((end (+ start delta 1))
                 (interval (subseq name (1+ start) end)))
            (values interval start end))))))

(defun first-name (list)
  (symbol-name (first list)))

(defun replace-section (symbol replace start &optional (end start))
  (let ((name (symbol-name symbol)))
    (intern (format nil "~A~A~A"
                    (subseq name 0 start)
                    replace
                    (subseq name (1+ end)))
            (symbol-package symbol))))
