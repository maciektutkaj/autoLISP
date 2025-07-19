(defun c:SDML( / p1 p2 p3 txtH txt txt1 p1x p1y p2x p2y distp1p2 jedn sobject)
	(command "_layer" "_m" "!GDX_OdchSytuacja" "_c" "7" """")
	(setq txtH (getreal "\nPodaj wysokosc tekstu: "))
	(setq jedn (getreal "\nWybierz jednostki: \n[m] = 1; [cm] = 100; [mm] = 1000 "))
	(princ)
	(while
		(setq p1 (getpoint "\nWskaz punkt pomierzony: "))
		(setq sobject (car (entsel "\nWybierz objekt projektowy: ")))
           (member (cdr (assoc 0 (entget sobject)))
                   '("LWPOLYLINE" "POLYLINE" "LINE" "3dPOLYLINE"))
		(setq p2 (vlax-curve-getClosestPointTo sobject p1 t))
		(setq p3 (getpoint "\nWskaz lokalizacje tekstu: "))
	(progn
		(setq p1x (car p1))
		(setq p1y (cadr p1))
		(setq p2x (car p2))
		(setq p2y (cadr p2))
		(setq distp1p2 (sqrt (+ (expt (- p2x p1x) 2) (expt (- p2y p1y) 2))))
		(setq txt (strcat (rtos (* distp1p2 jedn) 2 0)))
			(if (< (car p1) (car p2))
				(progn
					(setq txt1 (strcat "+" txt))
					(command "_text" p3 txtH "0" txt1)
				);progn
			);if
			(if (> (car p1) (car p2))
				(progn
					(setq txt1 (strcat "-" txt))
					(command "_text" p3 txtH "0" txt1)
				);progn
			);if
	);progn
);while
(princ)
)
(prompt "\nSDML")
