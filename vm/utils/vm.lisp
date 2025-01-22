(require "vm/utils/attr.lisp")

(defun is-debug(vm)
  (attr-get vm :DEBUG))

(defun set-debug(vm)
  (attr-set vm :DEBUG t))

(defun pc-get(vm)
  (attr-get vm :PC))

(defun pc-set(vm val)
  (attr-set vm :PC val))

(defun pc-incr(vm)
  (pc-set vm (+ (pc-get vm) 1)))

(defun pc-decr(vm)
  (pc-set vm (- (pc-get vm) 1)))

(defun bp-get(vm)
  (attr-get vm :BP))

(defun bp-set(vm val)
  (attr-set vm :BP val))

(defun sp-get(vm)
  (attr-get vm :SP))

(defun sp-set(vm val)
  (attr-set vm :SP val))

(defun fp-get(vm)
  (attr-get vm :FP))

(defun fp-set(vm val)
  (attr-set vm :FP val))

(defun is-jmp (insn)
  (if (member (first insn) '(JMP JSR JGT JGE JLT JLE JEQ JNE JTRUE JNIL))
      t
      nil))

(defun is-label(insn)
  (equal (first insn) 'LABEL))

(defconstant +start-code-id+ 0)
(defconstant +last-code-id+ 1)
(defconstant +etiq-id+ 2)
(defconstant +is-running-id+ 3)
(defconstant +ms-id+ 4)

(defun var-basse-get (vm id)
  (mem-get vm id))

(defun var-basse-set (vm id val)
  (mem-set vm id val))

(defun is-running(vm)
  (equal (var-basse-get vm +is-running-id+) 1))

(defun set-running(vm val)
  (var-basse-set vm +is-running-id+ val))

(defun ms-get(vm)
  (var-basse-get vm +ms-id+))

(defun ms-set(vm val)
  (var-basse-set vm +ms-id+ val))

(defun update-labels-for-jumps (vm)
  (let ((etiq-table (etiq-get-table vm))
        (last-code (var-basse-get vm +last-code-id+))
        (pc (pc-get vm)))
    (loop for addr from last-code to pc do
      (let ((insn (mem-get vm addr)))
        (when (and insn (is-jmp insn) (or (symbolp (second insn)) (stringp (second insn))))
          (let ((label (second insn)))
            (let ((label-addr (etiq-get vm label)))
              (if label-addr
                (mem-set vm addr (list (first insn) label-addr))))))))))

(defun etiq-get-table (vm)
  (var-basse-get vm +etiq-id+))

(defun etiq-get (vm label)
  (gethash (string label) (etiq-get-table vm)))

(defun etiq-set (vm label addr)
  (let ((etiq-table (etiq-get-table vm)))
    (setf (gethash (string label) etiq-table) addr)))

(defun is-etiq-set (vm label)
  (let ((etiq-table (etiq-get-table vm)))
    (if (gethash (string label) etiq-table)
        t
        nil)))

(defun stack-get (vm)
  (let ((bp (attr-get vm :BP))  ; Récupère la valeur de BP
        (sp (attr-get vm :SP))  ; Récupère la valeur de SP
        (mem (attr-get vm :mem)))  ; Récupère la mémoire complète
    (subseq mem (+ bp 1) (1+ sp))))  ; Renvoie la partie de la mémoire de BP à SP inclus
