(require "vm/vm.lisp")

;; Différents MOVE
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 10) :R0)
    (MOVE :R0 :R2)
  ))
  (vm-execute vm)
  (format t "Test MOVE CONST: ~A~%" (= (attr-get vm :R0) 10))
  (format t "Test MOVE: ~A~%" (= (attr-get vm :R2) 10)))

;; Différents MOVE
(let (vm '())
  (vm-init vm)
  (etiq-set vm 'a 10)
  (vm-load vm '(
    (MOVE (:@ a) :R0)
  ))
  (vm-execute vm)
  (format t "Test MOVE GLOBAL VAR: ~A~%" (= (attr-get vm :R0) 10)))