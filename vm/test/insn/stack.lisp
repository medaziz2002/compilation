(require "vm/vm.lisp")

;; Test du stack direct
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (PUSH (:CONST 5))
    (POP :R1)
  ))
  (vm-execute vm)
  (format t "Test PUSH/POP CONST: ~A~%" (= (attr-get vm :R1) 5))
)

;; Test du stack indirect
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (PUSH :R0)
    (POP :R1)
  ))
  (attr-set vm :R0 10)  ; Set R0 value to 10
  (vm-execute vm)
  (format t "Test PUSH/POP: ~A~%" (= (attr-get vm :R1) 10))
)
