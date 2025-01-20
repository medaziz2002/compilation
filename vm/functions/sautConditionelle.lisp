(defun process-jump (vm instruction)
  (let ((destination (cadr instruction)))
    (if (numberp destination)
        (update-pc vm (+ destination 1))
        (update-pc vm (+ (get-attr vm destination) 1)))))

(defun process-jsr (vm instruction)
  (let ((label (cadr instruction)))
    (if (or (numberp label) (is-label-set vm label))
        (progn
          (set-attr vm :R1 (- (get-pc vm) 1))
          (process-push vm '(PUSH :R1))
          (process-jump vm instruction))
        (if (fboundp (intern (string-upcase label)))
            (progn
              (let ((arguments '()))
                (let ((arg-count (memory-read vm (get-attr vm :SP))))
                  (dotimes (i arg-count)
                    (let ((arg-value (memory-read vm (- (get-attr vm :SP) (1+ i)))))
                      (when (is-debug-mode vm)
                        (format t "Argument: ~A~%" arg-value))
                      (push arg-value arguments)))
                  (let ((result (apply (intern (string-upcase label)) arguments)))
                    (set-attr vm :R0 result)))))
            (error "Undefined label: ~a" label)))))

(defun process-compare (vm instruction)
  (let ((operand1 (cadr instruction))
        (operand2 (caddr instruction)))
    (let ((value1 (cond 
                    ((is-const operand1) (cadr operand1))
                    ((keywordp operand1) (get-attr vm operand1))))
          (value2 (cond 
                    ((is-const operand2) (cadr operand2))
                    ((keywordp operand2) (get-attr vm operand2)))))
      (cond ((or (eq value1 't) (eq value1 'nil) (eq value2 't) (eq value2 'nil))
             (set-attr vm :EQ (if (eq value1 value2) 1 0))
             (set-attr vm :LT 0)
             (set-attr vm :GT 0))
            (t
             (set-attr vm :EQ (if (= value1 value2) 1 0))
             (set-attr vm :LT (if (< value1 value2) 1 0))
             (set-attr vm :GT (if (> value1 value2) 1 0)))))))

(defun process-jgt (vm instruction)
  (when (eq (get-attr vm :GT) 1)
    (process-jump vm instruction)))

(defun process-jge (vm instruction)
  (when (or (eq (get-attr vm :GT) 1) (eq (get-attr vm :EQ) 1))
    (process-jump vm instruction)))

(defun process-jlt (vm instruction)
  (when (eq (get-attr vm :LT) 1)
    (process-jump vm instruction)))

(defun process-jle (vm instruction)
  (when (or (eq (get-attr vm :LT) 1) (eq (get-attr vm :EQ) 1))
    (process-jump vm instruction)))

(defun process-jeq (vm instruction)
  (when (eq (get-attr vm :EQ) 1)
    (process-jump vm instruction)))

(defun process-jne (vm instruction)
  (when (eq (get-attr vm :EQ) 0)
    (process-jump vm instruction)))

(defun process-test (vm instruction)
  (let ((target (cadr instruction)))
    (let ((value (cond 
                  ((is-const target) (cadr target))
                  ((keywordp target) (get-attr vm target)))))
      (set-attr vm :IS-NIL (null value)))))

(defun process-jtrue (vm instruction)
  (when (not (get-attr vm :IS-NIL))
    (process-jump vm instruction)))

(defun process-jnil (vm instruction)
  (when (get-attr vm :IS-NIL)
    (process-jump vm instruction)))
