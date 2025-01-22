(defun attr-get(vm attr)
  (get vm attr))

(defun attr-set(vm attr val)
  (setf (get vm attr) val))