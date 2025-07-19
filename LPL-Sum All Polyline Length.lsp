(defun c:LPL (/ e ss l p i ZN)
	(setq ZN (getstring "\nCzy wstawic dlugosc na rysunku [T/N]: "))
	(if
    (setq l 0.0 ss (ssget '((0 . "LINE,SPLINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE"))))
	(progn
      (repeat (setq i (sslength ss))
        (setq e (ssname ss (setq i (1- i)))
              l (+ l (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
        )
      )
		(if (or (= ZN "t") (= ZN "t"))
			(progn
			  (if
				(setq p (getpoint "\nWskaz lokalizacje tekstu: "))
				(entmake
				  (list
					'(0 . "TEXT")
					'(100 . "AcDbText")
					(cons 10 (trans p 1 0))
					(cons 40 (/ 5.0 (getvar 'cannoscalevalue)))
					(cons 1 (rtos l 2 3))
				  )
				);entmake
				(princ (strcat "\nTotal length = " (rtos l 2 2)))
			  );if
			 );progn
			(alert (strcat "Dlugosc wskazanych elementw wynosi: " (rtos l 2 3)))
		)
    )
  )
  (princ)
)