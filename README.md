# axes
Axes is an extension to Common Lisp to support easy symbol insertion inside
a template. It's uses character @ (at) as keyword.

## Examples
Basic syntax inserts the desired symbol replacing @ :
```lisp
(do-axes (x y z)
  (incf (the-axis-@-in-foo foo) speed))

;;; expands to the equivalent of:
(progn
  (incf (the-axis-x-in-foo foo) speed)
  (incf (the-axis-y-in-foo foo) speed)
  (incf (the-axis-z-in-foo foo) speed)
  (values))
```

It is also possible to use insert multiple symbols, syntax becomes `@N@` where N
is the index.
```lisp
(do-axes ((horizontal x width) (vertical y height))
  (defun move-@1@ (foo)
    (incf (axis-@2@ foo) (size-@3@ foo))))

;;; expands to:
(progn
  (defun move-horizontal (foo)
    (incf (axis-x foo) (size-width foo)))
  (defun move-vertical (foo)
    (incf (axis-y foo) (size-height foo))))
```

Finally a hash-based symbol inserting is possible:
```lisp
(do-axes ((direction (horizontal vertical)) (size (width height)))
  (defun resize-@direction@ (value object)
    (incf (@size@ object) value)))

;;; expands to:
(progn
  (defun resize-horizontal (value object) (incf (width object) value))
  (defun resize-vertical (value object) (incf (height object) value)))
```
