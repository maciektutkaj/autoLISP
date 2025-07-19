(defun c:XYZlist ( / _MText normal pt )
  (defun _MText ( point text normal )
    (entmakex
      (list
        (cons 0 "MTEXT")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbMText")
        (cons 10 point)
        (cons 11 (getvar 'UCSXDIR))
        (cons 1  text)
        (cons 210 normal)
      )
    )
  )

  (setq normal (trans '(0. 0. 1.) 1 0 t))
  (terpri)
  (while (setq pt (getpoint "\rWskaz punkt: "))
    (setq pt (trans pt 1 0))
    (_MText pt
      (apply 'strcat
        (mapcar 'strcat '("Y = " "\nX = " "\nZ = ") (mapcar 'rtos pt))
      )
      normal
    )
  )
  (princ)
)