(require "vm/utils/vm.lisp")

;; Vérifie que pc-get/set/incr/decr fonctionne
(format t "Test IS-JUMP: ~A~%" (is-jmp '(JMP hello)))
(format t "Test IS-LABEL: ~A~%" (is-label '(LABEL label)))

;; Vérifie que pc-get/set/incr/decr fonctionne
(let (vm '())
  (pc-set vm 10)
  (pc-incr vm)
  (pc-incr vm)
  (pc-decr vm)
  (format t "Test PC-GET, PC-SET, PC-INCR, PC-DECR: ~A~%" (= (pc-get vm) 11)))

(format t "Test IS-JUMP: ~A~%" (is-jmp '(JMP hello)))
(format t "Test IS-LABEL: ~A~%" (is-label '(LABEL label)))