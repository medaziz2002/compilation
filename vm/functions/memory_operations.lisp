(require "vm/utils/require.lisp")

(defun execute-load (vm command)
  (let ((source (cadr command)) (destination (caddr command)))
    (cond
      ((numberp source)
       (set-attribute vm destination (read-memory vm source)))
      ((keywordp source)
       (set-attribute vm destination (read-memory vm (get-attribute vm source))))
      ((is-offset-format source)
        (let ((offset-value (caddr source)) (base-register (cadr source)))
          (set-attribute vm destination (read-memory vm (+ (get-attribute vm base-register) offset-value)))))
      ((is-global-variable source)
        (set-attribute vm destination (get-global-label vm (cadr source))))
      (t
       (format t "La source doit être un nombre, un registre, un décalage ou une variable globale : ~A~%" command)))))

(defun execute-store (vm command)
  (let ((source (cadr command)) (destination (caddr command)))
    (let ((mapped-source (cond
                          ((is-constant source) (cadr source))
                          ((keywordp source) (get-attribute vm source))
                          (t (format t "La source doit être une constante ou un registre : ~A~%" command))))
          (mapped-destination (cond
                               ((numberp destination) destination)
                               ((keywordp destination) (get-attribute vm destination))
                               ((is-offset-format destination)
                                (+ (caddr destination) (get-attribute vm (cadr destination))))))
          (is-global (is-global-variable destination)))
      (if is-global
          (set-global-label vm (cadr destination) mapped-source)
          (write-memory vm mapped-destination mapped-source)))))
