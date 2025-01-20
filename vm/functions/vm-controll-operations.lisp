(defun execute-nop (vm command)
  ;; Intentionally does nothing, representing a no-operation instruction.
  nil)

(defun execute-halt (vm command)
  (update-running vm 0))  ; Stops the execution of the VM
