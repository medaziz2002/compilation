(require "vm/vm.lisp")

;; Test du CMP
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 8) :R0)
    (DIV (:CONST 2) :R0)
    (CMP (:CONST 4) :R0)
    (MOVE :FEQ :R2)
  ))
  (vm-execute vm)
  (format t "Test CMP: ~A~%" (= (attr-get vm :R2) 1))
)

;; Test du test lol
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST nil) :R0)
    (TEST :R0)
  ))
  (vm-execute vm)
  (format t "Test TEST: ~A~%" (attr-get vm :FNIL))
)

;; Test de quelques sauts
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST nil) :R0)
    (TEST :R0)
    (JNIL fin)
    (INCR :R1)
    (LABEL fin)
  ))
  (vm-execute vm)
  (format t "Test JNIL: ~A~%" (= (attr-get vm :R1) 0))
)

(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 1) :R0)
    (TEST :R0)
    (JTRUE fin)
    (INCR :R1)
    (LABEL fin)
  ))
  (vm-execute vm)
  (format t "Test JTRIE: ~A~%" (= (attr-get vm :R1) 0))
)

;; Test de quelques sauts
(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 8) :R0)
    (DIV (:CONST 2) :R0)
    (CMP (:CONST 4) :R0)
    (JEQ fin)
    (INCR :R1)
    (LABEL fin)
  ))
  (vm-execute vm)
  (format t "Test JEQ: ~A~%" (= (attr-get vm :R1) 0))
)

(let ((vm '()))
  (vm-init vm)
  (vm-load vm '(
    (MOVE (:CONST 8) :R0)
    (DIV (:CONST 4) :R0)
    (CMP (:CONST 4) :R0)
    (JGT fin)
    (INCR :R1)
    (LABEL fin)
  ))
  (vm-execute vm)
  (format t "Test JGT: ~A~%" (= (attr-get vm :R1) 0))
)