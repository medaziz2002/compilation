(defun handle-push (vm instr)
  (let ((src (second instr)))
    (handle-incr vm (list 'INCR :SP))  ; Increment SP
    (handle-store vm (list 'STORE src :SP))  ; Store src at the new SP address
  )
)

(defun handle-pop (vm instr)
  (let ((dest (second instr)))
    (handle-load vm `(LOAD :SP ,dest))  ; Load the content of SP into dest
    (handle-decr vm `(DECR :SP))  ; Decrement SP
  )
)