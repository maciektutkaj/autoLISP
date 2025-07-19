(defun c:HD (/  p1 p2 x1 y1 z1 z1r x2 y2 z2 z2r dev B devdod)
	(command "_layer" "_m" "!GDX_dH" "_c" "7" """")
	(setq B (getreal "\nPodaj wysokosc tekstu: "))
 (while
    (setq p1 (getpoint "\nWskaz punkt pomierzony: "))
	(setq p2 (getpoint "\nWskaz punkt projektowy: "))
	(setq x1 (rtos (car p1) 2 3))
    (setq y1 (rtos (cadr p1) 2 3))
    (setq z1 (rtos (caddr p1) 2 3))
	(setq x2 (rtos (car p2) 2 3))
    (setq y2 (rtos (cadr p2) 2 3))
    (setq z2 (rtos (caddr p2) 2 3))
	(setq z1r (caddr p1))
	(setq z2r (caddr p2))
	(setq dev (strcat (rtos(* (- z1r z2r) 1000) 2 0)))
		(if (> (- z1r z2r) 0)
			(progn
			(setq devdod (strcat "+" "" dev ""))
			(command "_color" "1")
			(command "_text" p1 B "0" devdod )
			);progn
		);if
		(if (<= (- z1r z2r) 0)
		(progn
			(command "_color" "1")
			(command "_text" p1 B "0" dev )	
		);progn
		);if
  )
 )
 (princ "hd")