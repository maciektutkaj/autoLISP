
(defun c:Hlb (/  p x y ptcoord textloc)
  (while
    (setq p (getpoint "\nWskaz punkt : "))
    (setq textloc (getpoint "\nWskaz lokalizacje tekstu: "))
    (setq x (rtos (car p) 2 3))
    (setq y (rtos (cadr p) 2 3))
    (setq z (rtos (caddr p) 2 3))
    (setq ptcoord (strcat z))
    (command "_LEADER" p textloc "" ptcoord "")
  )
)