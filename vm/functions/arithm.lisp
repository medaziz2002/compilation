(defun execute-arithmetic-op (vm src dst operation)
  (let ((src-val (if (is-const src) (cadr src) (get-attr vm src)))
        (dst-val (get-attr vm dst)))
    (set-attr vm dst (funcall operation dst-val src-val))))

(defun process-addition (vm instruction)
  (execute-arithmetic-op vm (cadr instruction) (caddr instruction) #'+))

(defun process-subtraction (vm instruction)
  (execute-arithmetic-op vm (cadr instruction) (caddr instruction) #'-))

(defun process-multiplication (vm instruction)
  (execute-arithmetic-op vm (cadr instruction) (caddr instruction) #'*))

(defun process-division (vm instruction)
  (execute-arithmetic-op vm (cadr instruction) (caddr instruction) #'/))

(defun modify-increment-decrement (vm instruction operation)
  (let ((attribute (cadr instruction)))
    (when (keywordp attribute)
      (set-attr vm attribute (funcall operation (get-attr vm attribute) 1)))))

(defun process-increment (vm instruction)
  (modify-increment-decrement vm instruction #'+))

(defun process-decrement (vm instruction)
  (modify-increment-decrement vm instruction #'-))