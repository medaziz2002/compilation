(defun handle-move (vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (cond
      ((is-const src) (attr-set vm dst (second src)))
      ((is-global-var src) (attr-set vm dst (etiq-get vm (second src))))
      ((keywordp src) (attr-set vm dst (attr-get vm src))))))
  