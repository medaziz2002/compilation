



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

