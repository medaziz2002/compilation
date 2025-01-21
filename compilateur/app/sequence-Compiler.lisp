(defun compile-sequence (expressions ctx)
  ;; Cette fonction compile une séquence d'expressions et retourne leurs instructions
  (let ((instructions '())) ; Initialise une liste vide pour les instructions
    ;; Parcourt chaque expression de la séquence
    (dolist (expr expressions instructions)
      ;; Compile l'expression actuelle et ajoute ses instructions à la liste
      (setq instructions (append instructions (comp expr ctx))))
    instructions)) ; Retourne les instructions compilées


(defun compile-if (expr ctx)
  ;; Cette fonction compile une instruction conditionnelle `if`
  (let ((condition (first expr)) ; Extrait la condition
        (then-part (second expr)) ; Extrait la branche "then"
        (else-part (third expr)) ; Extrait la branche "else" (peut être nil)
        (etiq-else (generate-label)) ; Génère une étiquette pour la branche "else"
        (etiq-end (generate-label))) ; Génère une étiquette pour la fin
    (append
     (comp condition ctx) ; Compile la condition
     ;; Compare :R0 avec nil, saute à etiq-else si la condition est fausse
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-else))
     ;; Compile la branche "then"
     (comp then-part ctx)
     ;; Saute à la fin après "then"
     `((JMP ,etiq-end))
     ;; Étiquette pour la branche "else"
     `((LABEL ,etiq-else))
     (when else-part (comp else-part ctx)) ; Compile "else" si elle existe
     ;; Étiquette pour la fin
     `((LABEL ,etiq-end)))))

(defun compile-while (expr ctx)
  ;; Cette fonction compile une boucle `while`
  (let ((test (first expr)) ; Extrait la condition de la boucle
        (body (second expr)) ; Extrait le corps de la boucle
        (etiq-boucle (generate-label)) ; Génère une étiquette pour le début de la boucle
        (etiq-fin (generate-label))) ; Génère une étiquette pour la fin de la boucle
    (append
     `((LABEL ,etiq-boucle)) ; Étiquette de début de boucle
     (comp test ctx) ; Compile la condition
     ;; Si la condition est fausse, saute à la fin
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-fin))
     (comp body ctx) ; Compile le corps de la boucle
     ;; Retourne au début de la boucle
     `((JMP ,etiq-boucle))
     `((LABEL ,etiq-fin))))) ; Étiquette de fin

(defun compile-for (expr ctx)
  ;; Cette fonction compile une boucle `for`
  (let ((init (first expr)) ; Extrait l'initialisation
        (condition (second expr)) ; Extrait la condition
        (increment (third expr)) ; Extrait l'incrément
        (body (fourth expr)) ; Extrait le corps de la boucle
        (etiq-boucle (generate-label)) ; Génère une étiquette pour le début de la boucle
        (etiq-fin (generate-label))) ; Génère une étiquette pour la fin de la boucle
    (append
     (comp init ctx) ; Compile l'initialisation
     `((LABEL ,etiq-boucle)) ; Étiquette de début
     (comp condition ctx) ; Compile la condition
     ;; Si la condition est fausse, saute à la fin
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-fin))
     (comp body ctx) ; Compile le corps de la boucle
     (comp increment ctx) ; Compile l'incrément
     ;; Retourne au début de la boucle
     `((JMP ,etiq-boucle))
     `((LABEL ,etiq-fin))))) ; Étiquette de fin

(defun compile-cond (expr ctx)
  ;; Cette fonction compile une expression `cond`
  (let ((clauses expr) ; Liste des clauses (condition, corps)
        (etiq-end (generate-label)) ; Génère une étiquette pour la fin
        (instructions '())) ; Initialise les instructions vides
    ;; Parcourt chaque clause
    (dolist (clause clauses instructions)
      (let ((condition (first clause)) ; Extrait la condition
            (body (second clause)) ; Extrait le corps
            (etiq-next (generate-label))) ; Génère une étiquette pour la clause suivante
        (if (equal condition 't)
            ;; Si la condition est toujours vraie (t), compile directement le corps
            (setq instructions (append instructions (comp body ctx) `((JMP ,etiq-end))))
            ;; Sinon, compile la condition et vérifie si elle est vraie
            (progn
              (setq instructions (append instructions (comp condition ctx)))
              (setq instructions (append instructions `((CMP (:CONST nil) :R0) (JEQ ,etiq-next))))
              (setq instructions (append instructions (comp body ctx) `((JMP ,etiq-end))))
              (setq instructions (append instructions `((LABEL ,etiq-next))))))))
    (append instructions `((LABEL ,etiq-end))))) ; Ajoute l'étiquette de fin

    (defun compile-when (expr ctx)
  ;; Cette fonction compile une expression `when`
  (let ((condition (first expr)) ; Extrait la condition
        (body (rest expr)) ; Extrait le corps
        (etiq-end (generate-label))) ; Génère une étiquette pour la fin
    (append
     (comp condition ctx) ; Compile la condition
     ;; Si la condition est fausse, saute à la fin
     `((CMP (:CONST nil) :R0) (JEQ ,etiq-end))
     (compile-sequence body ctx) ; Compile le corps si la condition est vraie
     `((LABEL ,etiq-end))))) ; Étiquette de fin





