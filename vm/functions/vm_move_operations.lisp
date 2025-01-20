(defun execute-move (vm command)
  (let ((source (cadr command)) (destination (caddr command)))
    (cond
      ((is-constant source)
       (set-attribute vm destination (cadr source)))
      ((is-global-variable source)
       (set-attribute vm destination (get-global-label vm (cadr source))))
      ((keywordp source)
       (set-attribute vm destination (get-attribute vm source)))
      (t
       (format t "La source doit être une constante, une variable globale, ou un registre : ~A~%" command)))))
