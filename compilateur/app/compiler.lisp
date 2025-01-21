



(defun compile-addition (operands ctx)
  (append
    ;; Compilation du premier opérande
    (comp (first operands) ctx)

    ;; Compilation du deuxième opérande
    (comp (second operands) ctx)

    ;; Instructions pour effectuer l'addition avec un léger changement
    '((POP :R0)  ; Dépiler le premier opérande dans :R0 (inversé par rapport à l'original)
      (POP :R1)  ; Dépiler le deuxième opérande dans :R1
      (ADD :R0 :R1) ; Ajouter :R0 et :R1, stocker le résultat dans :R1
      (PUSH :R1)))) ; Empiler le résultat depuis :R1








(defun compile-multiplication (operands ctx)
  (append
    ;; Compilation du premier opérande
    (comp (first operands) ctx)

    ;; Compilation du deuxième opérande
    (comp (second operands) ctx)

    ;; Instructions pour effectuer la multiplication avec des ajustements
    '((POP :R0)  ; Dépiler le premier opérande dans :R0
      (POP :R1)  ; Dépiler le deuxième opérande dans :R1
      (MUL :R0 :R1) ; Multiplier :R0 et :R1, stocker le résultat dans :R1
      (PUSH :R1)))) ; Empiler le résultat depuis :R1     





(defun compile-division (operands ctx)
  (append
    ;; Compilation du premier opérande
    (comp (first operands) ctx)

    ;; Compilation du deuxième opérande
    (comp (second operands) ctx)

    ;; Instructions pour effectuer la division avec des ajustements
    '((POP :R1)  ; Dépiler le premier opérande dans :R1 (changement d'ordre)
      (POP :R0)  ; Dépiler le deuxième opérande dans :R0
      (DIV :R1 :R0) ; Diviser :R1 par :R0, stocker le résultat dans :R0
      (PUSH :R0)))) ; Empiler le résultat depuis :R0


    (defun comp-substutition (operands ctx)
  (append 
     ;; Compilation du premier opérande
     (comp (first operands) ctx)
          (comp (second operands) ctx)
          ;; Compilation du deuxième opérande
          '((POP :R1); Dépiler le premier opérande dans :R1 (changement d'ordre)
           (POP :R0) ; Dépiler le deuxième opérande dans :R0
          (SUB :R1 :R0) ; Diviser :R1 par :R0, stocker le résultat dans :R0
          (PUSH :R0)))); Empiler le résultat depuis :R0


    (defun compile-less-or-equal (operands ctx)
  ;; Cette fonction compile une comparaison "inférieur ou égal" (<=)
  (let ((etiq-true (generate-label)) ; Génère une étiquette pour la branche "vrai"
        (etiq-end (generate-label))) ; Génère une étiquette pour la fin de l'instruction
    (append
     (comp (car operands) ctx) ; Compile le premier opérande et génère les instructions nécessaires
     (comp (cadr operands) ctx) ; Compile le deuxième opérande
     '((POP :R1) ; Dépile le deuxième opérande de la pile et le place dans le registre :R1
       (POP :R0) ; Dépile le premier opérande de la pile et le place dans le registre :R0
       (CMP :R0 :R1)) ; Compare les deux opérandes : :R0 et :R1
     `((JLE ,etiq-true) ; Si :R0 <= :R1, saute à l'étiquette "etiq-true"
       (MOVE (:CONST nil) :R0) ; Sinon, stocke "nil" (faux) dans le registre :R0
       (JMP ,etiq-end) ; Saute directement à l'étiquette "etiq-end"
       (LABEL ,etiq-true) ; Étiquette "vrai" : représente le cas où :R0 <= :R1
       (MOVE (:CONST t) :R0) ; Stocke "t" (vrai) dans le registre :R0
       (LABEL ,etiq-end))))) ; Étiquette "fin" : point d'aboutissement de toutes les branches

    (defun compile-less-than (operands ctx)
  ;; Cette fonction compile une comparaison "strictement inférieur" (<)
  (let ((etiq-true (generate-label)) ; Génère une étiquette pour la branche "vrai"
        (etiq-end (generate-label))) ; Génère une étiquette pour la fin de l'instruction
    (append
     (comp (car operands) ctx) ; Compile le premier opérande
     (comp (cadr operands) ctx) ; Compile le deuxième opérande
     '((POP :R1) ; Dépile le deuxième opérande dans le registre :R1
       (POP :R0) ; Dépile le premier opérande dans le registre :R0
       (CMP :R0 :R1)) ; Compare les deux opérandes
     `((JLT ,etiq-true) ; Si :R0 < :R1, saute à "etiq-true"
       (MOVE (:CONST nil) :R0) ; Sinon, stocke "nil" (faux) dans :R0
       (JMP ,etiq-end) ; Saute à la fin
       (LABEL ,etiq-true) ; Étiquette pour la branche "vrai"
       (MOVE (:CONST t) :R0) ; Stocke "t" (vrai) dans :R0
       (LABEL ,etiq-end))))) ; Étiquette pour la fin

(defun compile-greater-than (operands ctx)
  ;; Cette fonction compile une comparaison "strictement supérieur" (>)
  (let ((etiq-true (generate-label)) ; Génère une étiquette pour la branche "vrai"
        (etiq-end (generate-label))) ; Génère une étiquette pour la fin de l'instruction
    (append
     (comp (car operands) ctx) ; Compile le premier opérande
     (comp (cadr operands) ctx) ; Compile le deuxième opérande
     '((POP :R1) ; Dépile le deuxième opérande dans :R1
       (POP :R0) ; Dépile le premier opérande dans :R0
       (CMP :R0 :R1)) ; Compare les deux opérandes
     `((JGT ,etiq-true) ; Si :R0 > :R1, saute à "etiq-true"
       (MOVE (:CONST nil) :R0) ; Sinon, stocke "nil" dans :R0
       (JMP ,etiq-end) ; Saute à la fin
       (LABEL ,etiq-true) ; Étiquette pour la branche "vrai"
       (MOVE (:CONST t) :R0) ; Stocke "t" dans :R0
       (LABEL ,etiq-end))))) ; Étiquette pour la fin


(defun compile-equal (operands ctx)
  ;; Cette fonction compile une comparaison "égalité" (==)
  (let ((etiq-true (generate-label)) ; Génère une étiquette pour la branche "vrai"
        (etiq-end (generate-label))) ; Génère une étiquette pour la fin de l'instruction
    (append
     (comp (car operands) ctx) ; Compile le premier opérande
     (comp (cadr operands) ctx) ; Compile le deuxième opérande
     '((POP :R1) ; Dépile le deuxième opérande dans :R1
       (POP :R0) ; Dépile le premier opérande dans :R0
       (CMP :R0 :R1)) ; Compare les deux opérandes
     `((JEQ ,etiq-true) ; Si :R0 == :R1, saute à "etiq-true"
       (MOVE (:CONST nil) :R0) ; Sinon, stocke "nil" dans :R0
       (JMP ,etiq-end) ; Saute à la fin
       (LABEL ,etiq-true) ; Étiquette pour la branche "vrai"
       (MOVE (:CONST t) :R0) ; Stocke "t" dans :R0
       (LABEL ,etiq-end))))) ; Étiquette pour la fin

