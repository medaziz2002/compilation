(defun extend-context-with-params (params ctx)
  ;; Ajoute les paramètres d'une fonction au contexte existant.
  ;; Chaque paramètre est associé à un offset basé sur la pile.
  (let ((offset (length ctx)))  ; Calcul initial de l'offset (taille du contexte)
    (dolist (param params ctx)  ; Parcours de tous les paramètres
      (setq ctx (cons (cons param (- offset 3)) ctx)) ; Associe chaque paramètre à son offset
      (decf offset))))  ; Réduit l'offset à chaque étape

(defun comp-fun (fun-name params body ctx)
  ;; Compile une définition de fonction en pseudo-code assembleur.
  (let ((new-ctx (extend-context-with-params params ctx))  ; Étend le contexte avec les paramètres
        (entry-label (string fun-name))  ; Label d'entrée basé sur le nom de la fonction
        (exit-label (concatenate 'string (string fun-name) "-exit")) ; Label de sortie
        (cleanup-instrs '()))  ; Instructions pour nettoyer la pile

    ;; Assemblage des instructions pour la définition de la fonction
    (append `((JMP ,exit-label)  ; Saut vers la fin pour éviter d'exécuter directement la fonction
              (LABEL ,entry-label))  ; Début de la fonction
            `((PUSH :FP) (MOVE :SP :FP))  ; Sauvegarde du Frame Pointer (FP) et mise à jour de la pile
            (comp body new-ctx)  ; Compile le corps de la fonction avec le contexte étendu
            cleanup-instrs  ; Instructions pour nettoyer la pile si nécessaire
            `((POP :R0) (POP :FP) (POP :R1) (JMP :R1))  ; Nettoyage et retour à l'appelant
            `((LABEL ,exit-label)))))  ; Label pour la fin de la fonction

(defun comp-call (fun-name args ctx)
  ;; Compile un appel de fonction en pseudo-code assembleur.
  (let ((call-instrs '()))  ; Initialisation des instructions pour l'appel
    ;; Compiler les arguments et les ajouter à `call-instrs`
    (dolist (arg args)
      (setq call-instrs (append call-instrs (comp arg ctx))))

    ;; Ajouter le nombre d'arguments
    (let ((nbArg (length args)))
      (setq call-instrs (append call-instrs `((MOVE (:CONST ,nbArg) :R0) (PUSH :R0)))))

    ;; Ajouter l'adresse de retour et l'instruction JSR (Jump to SubRoutine)
    (setq call-instrs (append call-instrs `((JSR ,fun-name))))

    ;; Nettoyer la pile après l'appel en dépilant les arguments
    (dolist (arg args)
      (setq call-instrs (append call-instrs '((POP :R1)))))

    ;; Récupérer le résultat sur la pile
    (setq call-instrs (append call-instrs `((POP :R1))))
    (setq call-instrs (append call-instrs '((PUSH :R0))))

    ;; Retourner les instructions de l'appel
    call-instrs))
