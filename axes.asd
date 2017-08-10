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

(asdf:defsystem #:axes
  :description "Used to insert symbols inside a template"
  :author "ReinUsesLisp <reinuseslisp@airmail.cc>"
  :license "LLGPL"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "multiple")
               (:file "individual")
               (:file "hashed")
               (:file "expansor")))
