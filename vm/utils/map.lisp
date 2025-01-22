(require "./attr.lisp")

(defun map-set(tab key val)
  (setf (gethash key tab) val))

(defun map-get(tab key)
  (gethash key tab))

(defun attr-map-init(vm attr size)
  (attr-set vm attr (make-hash-table :size size)))

(defun attr-map-get(vm attr key)
  (map-get (attr-get vm attr) key))

(defun attr-map-set(vm attr key val)
  (map-set (attr-get vm attr) key val))
