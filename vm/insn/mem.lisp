(require "vm/utils/require.lisp")

(defun handle-load (vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (cond
      ((numberp src) (attr-set vm dst (mem-get vm src)))
      ((keywordp src) (attr-set vm dst (mem-get vm (attr-get vm src))))
      ((is-offset src)
        (let ((offset (third src)) (attr (second src)))
          (attr-set vm dst (mem-get vm (+ (attr-get vm attr) offset)))))
      ((is-global-var src)
        (attr-set vm dst (etiq-get vm (second src))))
      (t (format t "La source doit être soit un nombre, soit un registre, soit un offset: ~A~%" insn)))))

(defun handle-store (vm insn)
  (let ((src (second insn)) (dst (third insn)))
    (let ((srcMapped (cond
                      ((is-const src) (second src))
                      ((keywordp src) (attr-get vm src))
                      (t (format t "La source doit être soit une constante, soit un registre: ~A~%" insn))))
          (dstMapped (cond
                      ((numberp dst) dst)
                      ((keywordp dst) (attr-get vm dst))
                      ((is-offset dst) (+ (third dst) (attr-get vm (second dst))))))
          (isGlobalVar (is-global-var dst)))
      (if isGlobalVar
          (etiq-set vm (second dst) srcMapped)
          (mem-set vm dstMapped srcMapped)))))
