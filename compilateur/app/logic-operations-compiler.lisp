(defun compile-logical-and (expr ctx)
  ;; Compile une expression logique AND
  (let ((etiq-false (generate-label))
        (etiq-end (generate-label))
        (first-expr (first expr))
        (second-expr (second expr)))
    (append
     (comp first-expr ctx) ; Compile la première expression
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-false)) ;; Si la première est false, saut vers etiq-false
     (comp second-expr ctx) ; Compile la deuxième expression
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-false)) ;; Si la deuxième est false, saut vers etiq-false
     `((MOVE (:CONST t) :R0) (JMP ,etiq-end) (PUSH :R0)) ;; Si les deux sont true, R0=t (true)
     `((LABEL ,etiq-false) (MOVE (:CONST 0) :R0) (PUSH :R0)) ;; Sinon, R0=0 (false)
     `((LABEL ,etiq-end))))) ; Fin de l'instruction AND

(defun compile-logical-or (expr ctx)
  ;; Compile une expression logique OR
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label))
        (first-expr (first expr))
        (second-expr (second expr)))
    (append
     (comp first-expr ctx) ; Compile la première expression
     `((CMP (:CONST t) :R0) (JEQ ,etiq-true)) ;; Si la première est true, saut vers etiq-true
     (comp second-expr ctx) ; Compile la deuxième expression
     `((CMP (:CONST t) :R0) (JEQ ,etiq-true)) ;; Si la deuxième est true, saut vers etiq-true
     `((MOVE (:CONST nil) :R0) (JMP ,etiq-end) (PUSH :R0)) ;; Si les deux sont false, R0=nil (false)
     `((LABEL ,etiq-true) (MOVE (:CONST t) :R0) (PUSH :R0)) ;; Sinon, R0=t (true)
     `((LABEL ,etiq-end))))) ; Fin de l'instruction OR

(defun compile-logical-not (expr ctx)
  ;; Compile une expression logique NOT
  (let ((etiq-true (generate-label))
        (etiq-end (generate-label))
        (first-expr (first expr)))
    (append
     (comp first-expr ctx) ; Compile l'expression
     `((CMP (:CONST t) :R0) (JEQ ,etiq-true)) ;; Si l'expression est true, saut vers etiq-true
     `((MOVE (:CONST t) :R0) (JMP ,etiq-end) (PUSH :R0)) ;; Si ce n'était pas true, R0=t (true)
     `((LABEL ,etiq-true) (MOVE (:CONST nil) :R0) (PUSH :R0)) ;; Sinon, R0=nil (false)
     `((LABEL ,etiq-end))))) ; Fin de l'instruction NOT
