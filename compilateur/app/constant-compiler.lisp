(defun compile-constante (expr ctx)
  `((MOVE (:CONST ,expr) :R0) (PUSH :R0)))