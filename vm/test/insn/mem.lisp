(require "vm/vm.lisp")

;; Différents store
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (STORE (:CONST 2) :R0)
    (STORE :R1 2)
    (STORE (:CONST 10) (+ :R0 2))
  ))
  (mem-set vm 1 2)
  (attr-set vm :R0 1)
  (attr-set vm :R1 4)
  (vm-execute vm)
  (format t "Test STORE DIRECT: ~A~%" (= (mem-get vm 1) 2))
  (format t "Test STORE INDIRECT: ~A~%" (= (mem-get vm 2) 4))
  (format t "Test STORE INDIRECT+OFFSET: ~A~%" (= (mem-get vm 3) 10)))