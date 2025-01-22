(require "utils/require.lisp")
(require "vm/insn/require.lisp")

;; —————————————   MAX_MEM
;; Zone code (1/2) PC
;; —————————————   MS
;; Zone pile (1/2) SP
;; —————————————   BP
;; Variables basses (30)
;; ————————————— 0
(defun vm-reset(vm &optional (size 1000))
  (let ((size (max size 1000)) (variablesBasse 30) (tailleZones (- (max size 1000) 30)))
    (attr-set vm :R0 0)
    (attr-set vm :R1 0)
    (attr-set vm :R2 0)
    (attr-set vm :MAX_MEM size)          ;; Définition de la taille de la VM
    (attr-array-init vm :MEM size)       ;; Définition de la mémoire
    (var-basse-set vm +start-code-id+ (- size 1))
    (var-basse-set vm +etiq-id+ (make-hash-table))
    (pc-set vm (- size 1))               ;; puis on va diminuer dans la mémoire, ça permet de ne pas trop se faire de soucis
    (bp-set vm 30)                       ;; Le BP lui est défini après les variables basses.
    (sp-set vm (bp-get vm))              ;; Le stack pointer est de base sur BP.
    (fp-set vm (sp-get vm))
    (ms-set vm (+ variablesBasse (/ tailleZones 2))) ;; s'en suit la valeur maximum du stack qu'on ne doit pas dépasser
    (set-running vm 1)))                 ;; Ainsi pour une VM taille 1000: BP = 30, SP = 30, MS = 224, MAX_MEM = 1000

(defun vm-init(vm &optional (size 1000))
  (attr-set vm :NAME vm)
  (vm-reset vm))

(defun vm-load (vm program)
    ;; Détermine l'adresse de départ pour charger le programme
  (let ((initial-pc (- (or (var-basse-get vm +last-code-id+) (+ (pc-get vm) 1)) 1)))
        ;; Charge les instructions et les labels
        (loop for insn in program do
            (if (is-label insn)
                ;; Si c'est un label, stocke son adresse dans la table des labels
                (etiq-set vm (string (second insn)) initial-pc)
                ;; Sinon, stocke l'instruction en mémoire et met à jour initial-pc
                (progn 
                    (mem-set vm initial-pc insn)
                    (setq initial-pc (- initial-pc 1)))))

        ;; Met à jour :LAST_CODE
        (var-basse-set vm +last-code-id+ (+ initial-pc 1))

        ;; Mise à jour des adresses pour les sauts
        (update-labels-for-jumps vm)))

(defun vm-execute (vm)
  (loop while (and (>= (pc-get vm) (var-basse-get vm +last-code-id+)) (is-running vm)) do
    (let ((insn (mem-get vm (pc-get vm))))
      (if (is-debug vm) (format t "~A " insn))
      (cond
        ((equal (first insn) 'LOAD) (handle-load vm insn))
        ((equal (first insn) 'STORE) (handle-store vm insn))
        ((equal (first insn) 'MOVE) (handle-move vm insn))
        ((equal (first insn) 'ADD) (handle-add vm insn))
        ((equal (first insn) 'SUB) (handle-sub vm insn))
        ((equal (first insn) 'MUL) (handle-mul vm insn))
        ((equal (first insn) 'DIV) (handle-div vm insn))
        ((equal (first insn) 'INCR) (handle-incr vm insn))
        ((equal (first insn) 'DECR) (handle-decr vm insn))
        ((equal (first insn) 'PUSH) (handle-push vm insn))
        ((equal (first insn) 'POP) (handle-pop vm insn))
        ((equal (first insn) 'NOP) (handle-nop vm insn))
        ((equal (first insn) 'HALT) (handle-halt vm insn))
        ((equal (first insn) 'JMP) (handle-jmp vm insn))
        ((equal (first insn) 'CMP) (handle-cmp vm insn))
        ((equal (first insn) 'JSR) (handle-jsr vm insn))
        ((equal (first insn) 'JGT) (handle-jgt vm insn))
        ((equal (first insn) 'JGE) (handle-jge vm insn))
        ((equal (first insn) 'JLT) (handle-jlt vm insn))
        ((equal (first insn) 'JLE) (handle-jle vm insn))
        ((equal (first insn) 'JEQ) (handle-jeq vm insn))
        ((equal (first insn) 'JNE) (handle-jne vm insn))
        ((equal (first insn) 'TEST) (handle-test vm insn))
        ((equal (first insn) 'JNIL) (handle-jnil vm insn))
        ((equal (first insn) 'JTRUE) (handle-jtrue vm insn))
        (t (format t "Instruction inconnue: ~A~%" insn)))
      (pc-decr vm)
      (if (is-debug vm)
        (format t "R0: ~A R1: ~A R2: ~A SP: ~A FP: ~A Stack: ~A~%"
                (attr-get vm :R0)
                (attr-get vm :R1)
                (attr-get vm :R2)
                (attr-get vm :SP)
                (attr-get vm :FP)
                (stack-get vm))))))