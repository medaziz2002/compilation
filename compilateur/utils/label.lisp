(defvar *label-counter* 0 "Compteur global pour la génération d'étiquettes.")

(defun generate-label()
  (incf *label-counter*)
  (format nil "LABEL~A" *label-counter*))