# axes
Axes is an extension to Common Lisp to support easy symbol insertion inside
a template. It's uses character @ (at) as keyword.

## Examples
Basic syntax inserts the desired symbol replacing @ :
```lisp
(progn-axes (x y z)
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
(progn-axes ((horizontal x width) (vertical y height))
  (defun move-@1@ (foo)
    (incf (axis-@2@ foo) (size-@3@ foo))))

;;; expands to:
(progn
  (defun move-horizontal (foo)
    (incf (axis-x foo) (size-width foo)))
  (defun move-vertical (foo)
    (incf (axis-y foo) (size-height foo))))
```

It's possible to map return values, like in the following example:
```lisp
(let ((x -5) (y 5))
  (map-axes (x y)
    (+ @ 5)))
=> (0 10)
```
