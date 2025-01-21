((require "compilateur/app/compiler.lisp")

    
defun comp (expr &optional (ctx '()))
  (cond
    ((symbolp expr) (comp-var expr ctx)) ; Compilation des variables
    ((atom expr) (comp-const expr ctx))  ; Compilation des constantes
    ((listp expr)                    ; Compilation des expressions de liste
     (cond
       ((equal (first expr) '+) (compile-addition (cdr expr) ctx))
       ((equal (first expr) '-) (comp-sub (cdr expr) ctx))
       ((equal (first expr) '/) (comp-div (cdr expr) ctx))
       ((equal (first expr) '*) (comp-mul (cdr expr) ctx))
       ((equal (first expr) '>=) (comp-ge (cdr expr) ctx))
       ((equal (first expr) '<=) (comp-le (cdr expr) ctx))
       ((or (equal (first expr) 'equal) (equal (first expr) '=)) (comp-eq (cdr expr) ctx))
       ((equal (first expr) '>) (comp-gt (cdr expr) ctx))
       ((equal (first expr) '<) (comp-lt (cdr expr) ctx))
       ((equal (first expr) 'and) (comp-and (cdr expr) ctx))
       ((equal (first expr) 'or) (comp-or (cdr expr) ctx))
       ((equal (first expr) 'not) (comp-not (cdr expr) ctx))
       ((equal (first expr) 'progn) (comp-seq (cdr expr) ctx))
       ((equal (first expr) 'if) (comp-if (cdr expr) ctx))
       ((equal (first expr) 'while) (comp-while (cdr expr) ctx))
       ((equal (first expr) 'for) (comp-for (cdr expr) ctx))
       ((equal (first expr) 'cond) (comp-cond (cdr expr) ctx))
       ((equal (first expr) 'when) (comp-when (cdr expr) ctx))
       ((equal (first expr) 'setf) (comp-setf (cdr expr) ctx))
       ((equal (first expr) 'let) (comp-let (second expr) (third expr) ctx))
       ((equal (first expr) 'defun) (comp-fun (second expr) (third expr) (fourth expr) ctx))
       ((symbolp (first expr)) (comp-call (first expr) (rest expr) ctx))
       (t (format t "Expression impossible à compiler: ~A~%" expr)) ; Gestion des erreurs
     ))
  )
)
