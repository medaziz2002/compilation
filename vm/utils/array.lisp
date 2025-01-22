(require "./attr.lisp")

(defun array-get(tab key)
  (aref tab key))

(defun array-set(tab key val)
  (setf (aref tab key) val))

(defun attr-array-init(vm attr size)
  (attr-set vm attr (make-array size)))

(defun attr-array-get(vm attr key)
  (array-get (attr-get vm attr) key))

(defun attr-array-set(vm attr key val)
  (array-set (attr-get vm attr) key val))
