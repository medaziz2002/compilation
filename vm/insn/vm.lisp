(defun handle-nop(vm insn)
  ;; On ne fait rien intentionnellement.
)

(defun handle-halt(vm insn)
  (set-running vm 0))