(defun handle-jmp (vm insn)
  (let ((target (second insn)))
    (if (numberp target)
        (pc-set vm (+ target 1))
        (pc-set vm (+ (attr-get vm target) 1)))))

(defun handle-jsr (vm insn)
  ;; Extraire l'étiquette de l'instruction
  (let ((label (second insn)))
    (if (or (numberp label) (is-etiq-set vm label))
        (progn
          ;; Si l'étiquette est définie, continuer avec l'exécution normale
          (attr-set vm :R1 (- (pc-get vm) 1))
          (handle-push vm '(PUSH :R1))
          (handle-jmp vm insn))
        ;; Gérer le cas où l'étiquette n'est pas définie
        (if (fboundp (intern (string-upcase label)))
            (progn
              ;; Si label est une fonction Lisp, récupérer les arguments et appeler la fonction
              (let ((args '()))
                ;; Récupérer le nombre d'arguments du stack
                (let ((arg-count (mem-get vm (attr-get vm :SP))))
                  ;; Récupérer les arguments du stack
                    (dotimes (i arg-count)
                    (let ((arg-value (mem-get vm (- (attr-get vm :SP) (+ i 1)))))
                      (if (is-debug vm)
                          (format t "Arg: ~A~%" arg-value))
                      (push arg-value args)))
                  ;; Appeler la fonction Lisp avec les arguments et stocker le résultat dans R0
                  (let ((result (apply (intern (string-upcase label)) args)))
                    (attr-set vm :R0 result)))))
            ;; Sinon, signaler une erreur
            (error "Etiquette non définie: ~a" label)))))

(defun handle-cmp (vm insn)
  (let ((reg1 (second insn))
        (reg2 (third insn)))
    (let ((val1 (cond 
                  ((is-const reg1) (second reg1))
                  ((keywordp reg1) (attr-get vm reg1))))
          (val2 (cond 
                  ((is-const reg2) (second reg2))
                  ((keywordp reg2) (attr-get vm reg2)))))
      ;; Gérer les cas où val1 ou val2 sont t ou nil
      (cond ((or (eq val1 't) (eq val1 'nil) (eq val2 't) (eq val2 'nil))
             ;; Comparaison d'égalité seulement
             (attr-set vm :FEQ (if (eq val1 val2) 1 0))
             (attr-set vm :FLT 0)
             (attr-set vm :FGT 0))
            ;; Cas normal
            (t
             (attr-set vm :FEQ (if (= val1 val2) 1 0))
             (attr-set vm :FLT (if (< val1 val2) 1 0))
             (attr-set vm :FGT (if (> val1 val2) 1 0)))))))


(defun handle-jgt (vm insn)
  (if (eq (attr-get vm :FGT) 1)
      (handle-jmp vm insn)))

(defun handle-jge (vm insn)
  (if (or (eq (attr-get vm :FGT) 1) (eq (attr-get vm :FEQ) 1))
      (handle-jmp vm insn)))

(defun handle-jlt (vm insn)
  (if (eq (attr-get vm :FLT) 1)
      (handle-jmp vm insn)))

(defun handle-jle (vm insn)
  (if (or (eq (attr-get vm :FLT) 1) (eq (attr-get vm :FEQ) 1))
      (handle-jmp vm insn)))

(defun handle-jeq (vm insn)
  (if (eq (attr-get vm :FEQ) 1)
      (handle-jmp vm insn)))

(defun handle-jne (vm insn)
  (if (eq (attr-get vm :FEQ) 0)
      (handle-jmp vm insn)))

(defun handle-test(vm insn)
  (let ((dst (second insn)))
    (let ((v (cond 
            ((is-const dst) (second dst))
            ((keywordp dst) (attr-get vm dst)))))
      (attr-set vm :FNIL (null v)))))

(defun handle-jtrue (vm insn)
  (if (not (attr-get vm :FNIL))
    (handle-jmp vm insn)))

(defun handle-jnil (vm insn)
  (if (attr-get vm :FNIL)
    (handle-jmp vm insn)))