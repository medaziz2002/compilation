(defun compile-variable (var ctx)
  (let ((local-offset (cdr (assoc var ctx))))
    (if local-offset
        ;; Charger une variable locale en utilisant son offset
        `((LOAD (+ :FP ,local-offset) :R0) (PUSH :R0))
        ;; Charger une variable globale
        `((LOAD (:@ ,var) :R0) (PUSH :R0)))))
