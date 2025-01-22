
(require "app/constant-compiler.lisp")
(require "app/function-compiler.lisp")
(require "app/logic-operations-compiler.lisp")
(require "app/variable-compiler.lisp")
(require "app/sequence-compiler.lisp")  ;; Vérification de la casse
(require "utils/label.lisp")
(require "app/arithmetic-compiler.lisp")

(defun comp (expr &optional (ctx '()))
  (cond
    ;; Compilation des variables
    ((symbolp expr) (compile-variable expr ctx))
    
    ;; Compilation des constantes
    ((atom expr) (compile-constante expr ctx))
    
    ;; Compilation des expressions de liste
    ((listp expr)
     (cond
       ;; Cas pour chaque opération
       ((equal (first expr) '+) (compile-addition (cdr expr) ctx))
       ((equal (first expr) '-) (compile-subtraction (cdr expr) ctx))
       ((equal (first expr) '/) (compile-division (cdr expr) ctx))
       ((equal (first expr) '*) (compile-multiplication (cdr expr) ctx))
       ;; Comparaisons
       ((equal (first expr) '>=) (compile-greater-or-equal (cdr expr) ctx))
       ((equal (first expr) '<=) (compile-less-or-equal (cdr expr) ctx))
       ((or (equal (first expr) 'equal) (equal (first expr) '=)) (compile-equal (cdr expr) ctx))
       ((equal (first expr) '>) (compile-greater-than (cdr expr) ctx))
       ((equal (first expr) '<) (compile-less-than (cdr expr) ctx))
       ;; Opérations logiques
       ((equal (first expr) 'and) (compile-logical-and (cdr expr) ctx))
       ((equal (first expr) 'or) (compile-logical-or (cdr expr) ctx))
       ((equal (first expr) 'not) (compile-logical-not (cdr expr) ctx))
       ;; Expressions de contrôle
       ((equal (first expr) 'progn) (compile-sequence (cdr expr) ctx))
       ((equal (first expr) 'if) (compile-if (cdr expr) ctx))
       ((equal (first expr) 'while) (compile-while (cdr expr) ctx))
       ((equal (first expr) 'for) (compile-for (cdr expr) ctx))
       ((equal (first expr) 'cond) (compile-cond (cdr expr) ctx))
       ((equal (first expr) 'when) (compile-when (cdr expr) ctx))
       ;; Affectation et déclaration
       ((equal (first expr) 'setf) (compile-setf (cdr expr) ctx))
       ((equal (first expr) 'let) (compile-let (second expr) (third expr) ctx))
       ;; Définition de fonctions
       ((equal (first expr) 'defun) (compile-function (second expr) (third expr) (fourth expr) ctx))
       ;; Appels de fonctions utilisateur
       ((symbolp (first expr)) (compile-function-call (first expr) (rest expr) ctx))
       ;; Erreur si l'expression est inconnue
       (t (format t "Expression impossible à compiler: ~A~%" expr)))))) ;; Cette ligne était incorrecte, parenthèse mal placée
