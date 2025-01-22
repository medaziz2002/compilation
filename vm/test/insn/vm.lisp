(require "vm/vm.lisp")

;; Vérifie que le programme s'arrête
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 10) :R0)
    (HALT)
    (INCR :R0)
  ))
  (vm-execute vm)
  (format t "Test HALT CONST: ~A~%" (= (attr-get vm :R0) 10)))

;; Un programme vraiment complexe pour le coup
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (JMP target)
    (MOVE (:CONST 1) :R1)
  ))
  (vm-load vm '(
    (LABEL target)
    (MOVE (:CONST 2) :R2)
  ))
  (vm-execute vm)
  (format t "Test chargement deux programmes : ~A~%" (and (= (attr-get vm :R2) 2) (= 0 (attr-get vm :R1))))
)
