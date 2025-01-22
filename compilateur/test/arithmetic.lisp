(require "compilateur/app/arithmetic-compiler.lisp")



(let ((vm '()) (program '(- (* 20 8) (/ 80 4))))
  (vm-init vm)
  (vm-load vm (comp program))
  (vm-execute vm)
  (format t "Test: Expression arithmétique ~A = 144? ~A~%" program (= 144 (attr-get vm :R0))))

(let ((vm '()) (program '(- (* 10 5) (/ 50 2))))
  (vm-init vm)
  (vm-load vm (comp program))
  (vm-execute vm)
  (format t "Test: Calcul avec variable ~A = 75? ~A~%" program (= 75 (attr-get vm :R0))))

(let ((vm '()) (program '(<= 7 10)))
  (vm-init vm)
  (vm-load vm (comp program))
  (format t "Compilation des instructions: ~A~%" (comp program))
  (vm-execute vm)
  (format t "Test: Comparaison ~A donne ~A~%" program (attr-get vm :R0))))
