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

(defmacro do-axes (axes &body body)
  `(,(find-axes-mode axes) ,axes ,@body))          

(defmacro do-axes-multiple (axes &body body)
  (check-multiple-axes axes)
  `(progn
     ,@(loop for axis in axes
          collect `(progn
                     ,@(operate-sexp body #'operate-symbol-multiple axis)))
     (values)))

(defmacro do-axes-individual (axes &body body)
  (check-individual-axes axes)
  `(progn
     ,@(loop for axis in axes
          collect `(progn
                     ,@(operate-sexp body #'operate-symbol-individual axis)))
     (values)))

(defmacro do-axes-hashed (axes &body body)
  `(progn
     ,@(loop for i from 0 upto (1- (length (second (first axes))))
          collect `(progn
                     ,@(operate-sexp body #'operate-symbol-hashed axes i)))
     (values)))

(defun operate-sexp (sexp operate &rest data)
  (cond
    ((listp sexp)
     (if (member (first sexp) '(do-axes map-axes))
         sexp                           ; guard nested expansion
         (loop for sub-sexp in sexp
            collect (apply #'operate-sexp `(,sub-sexp ,operate ,@data)))))
    ((symbolp sexp) (apply operate `(,sexp ,sexp ,@data)))
    (t sexp)))

(defun operate-symbol-individual (symbol original-symbol replace)
  (let* ((name (symbol-name symbol))
         (start (position #\@ name)))
    (if start
        (operate-symbol-individual
         (replace-section symbol replace start)
         original-symbol
         replace)
        symbol)))

(defun operate-symbol-multiple (symbol original-symbol axis)
  (declare (symbol symbol original-symbol) (list axis))
  (multiple-value-bind (interval start end)
      (find-interval symbol original-symbol)
    (if (null interval)
        symbol
        (let ((index (parse-integer interval)))
          ;; check if index is not out of scope
          (unless (< -1 (1- index) (length axis))
            (error "Invalid index ~D in symbol ~A." index original-symbol))
          (operate-symbol-multiple
           (replace-section symbol (nth (1- index) axis) start end)
           original-symbol
           axis)))))

(defun operate-symbol-hashed (symbol original-symbol axes index)
  (multiple-value-bind (interval start end)
      (find-interval symbol original-symbol)
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
           axes
           index)))))

(defun first-name (list)
  (symbol-name (first list)))

(defun replace-section (symbol replace start &optional (end start))
  (let ((name (symbol-name symbol)))
    (intern (format nil "~A~A~A"
                    (subseq name 0 start)
                    replace
                    (subseq name (1+ end)))
            (symbol-package symbol))))

(defun check-multiple-axes (axes)
  (when (reduce #'/= axes :key #'length)
    (error "Mixed sizes in axes ~A" axes)))

(defun check-individual-axes (axes)
  (declare (ignore axes)))

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

(defun find-axes-mode (axes)
  (cond
    ((and (every #'listp axes)
          (every #'symbolp (mapcar #'first axes))
          (every #'listp (mapcar #'second axes)))
     'do-axes-hashed)
    ((every #'symbolp axes) 'do-axes-individual)
    ((every #'listp axes) 'do-axes-multiple)
    (t (error "Unknown axes mode ~A" axes))))
