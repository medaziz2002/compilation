(require "vm/utils/array.lisp")

;; Vérifie que array-set/get, attr-array-set/get fonctionne
(let (vm '())
  (attr-array-init vm :MEM 5)
  (attr-array-set vm :MEM 0 300)
  (format t "Test ATTR-ARRAY-SET, ATTR-ARRAY-GET: ~A~%" (= (attr-array-get vm :MEM 0) 300)))