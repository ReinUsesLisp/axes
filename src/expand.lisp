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
  `(expand-axes ,axes (progn) ,@body))

(defmacro map-axes (axes &body body)
  `(expand-axes ,axes (list) ,@body))

(defmacro expand-axes (axes (&optional (function 'progn) (char #\@)) &body body)
  `(,(find-axes-mode axes) ,axes
     ,function
     ,(cast-to-character char)
     ,@body))

(defun find-axes-mode (axes)
  (cond
    ((and (every #'listp axes)
          (every #'symbolp (mapcar #'first axes))
          (every #'listp (mapcar #'second axes)))
     'do-axes-hashed)
    ((every #'symbolp axes) 'do-axes-individual)
    ((every #'listp axes) 'do-axes-multiple)
    (t (error "Unknown axes mode ~A" axes))))

(defun operate-sexp (sexp char operate data)
  (cond
    ((listp sexp)
     (if (not (member (first sexp) '(do-axes map-axes expand-axes)))
         (loop for sub-sexp in sexp
            collect (operate-sexp sub-sexp char operate data))
         ;; guard nested expansion
         (list* (first sexp)
                (operate-sexp (second sexp) char operate data)
                (cddr sexp))))
    ((symbolp sexp) (apply operate (list* sexp sexp char data)))
    (t sexp)))
