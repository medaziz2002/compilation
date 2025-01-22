(require "vm/utils/attr.lisp")

;; Vérifie que attr-set/get fonctionne
(let (vm '())
  (attr-set vm :R0 10)
  (format t "Test ATTR-SET, ATTR-GET: ~A~%" (= (attr-get vm :R0) 10)))
