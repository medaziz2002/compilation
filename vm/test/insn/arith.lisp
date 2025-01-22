(require "vm/vm.lisp")

;; Différents ADD
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (ADD (:CONST 10) :R0)
    (MOVE (:CONST 10) :R1)
    (ADD :R0 :R1)
  ))
  (vm-execute vm)
  (format t "Test ADD CONST: ~A~%" (= (attr-get vm :R0) 10))
  (format t "Test ADD: ~A~%" (= (attr-get vm :R1) 20)))

;; Différents SUB
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (SUB (:CONST 10) :R0)
    (MOVE (:CONST 10) :R1)
    (SUB :R0 :R1)
  ))
  (vm-execute vm)
  (format t "Test SUB CONST: ~A~%" (= (attr-get vm :R0) -10))
  (format t "Test SUB: ~A~%" (= (attr-get vm :R1) 20)))

;; Différents MUL
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 2) :R0)
    (MUL (:CONST 10) :R0)
    (MOVE (:CONST 10) :R1)
    (MUL :R0 :R1)
  ))
  (vm-execute vm)
  (format t "Test MUL CONST: ~A~%" (= (attr-get vm :R0) 20))
  (format t "Test MUL: ~A~%" (= (attr-get vm :R1) 200)))

;; Différents DIV
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 10) :R0)
    (DIV (:CONST 2) :R0)
    (MOVE (:CONST 5) :R1)
    (DIV :R0 :R1)
  ))
  (vm-execute vm)
  (format t "Test DIV CONST: ~A~%" (= (attr-get vm :R0) 5))
  (format t "Test DIV: ~A~%" (= (attr-get vm :R1) 1)))

;; INCR
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 10) :R0)
    (INCR :R0)
  ))
  (vm-execute vm)
  (format t "Test INCR: ~A~%" (= (attr-get vm :R0) 11)))

;; DECR
(let (vm '())
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 10) :R0)
    (DECR :R0)
  ))
  (vm-execute vm)
  (format t "Test DECR: ~A~%" (= (attr-get vm :R0) 9)))