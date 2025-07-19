;DFM - Deviation From Model Auto
(defun c:dfma ( / srf vsrf pts len ctr p nm vp pno x y elv srfz depth elvx srfzx txtH txtpos jedn dev )
    
  (setq txtH (getreal "\nPodaj wysokosc tekstu: "))
  (setq jedn (getreal "\nWybierz jednostki: \n[m] = 1; [cm] = 100; [mm] = 1000 "))
  (setq srf (car (entsel "\nWyskaz powierzchnie: "))
	vsrf (vlax-ename->vla-object srf)
	)
  (prompt "\nWybierz punkty z pomiaru: ")
  
  
  (setq pts (ssget)
	len (sslength pts)
	ctr 0)
	(setq nm (trans '(0. 0. 1.) 1 0 t))
  (while (< ctr len)

    (setq p (ssname pts ctr))

    (if (= (cdr (assoc 0 (entget p))) "AECC_COGO_POINT")
      (progn
	(setq vp (vlax-ename->vla-object p)
	      pno (itoa (vlax-get-property vp 'Number))
	      x (vlax-get-property vp 'Easting)
	      y (vlax-get-property vp 'Northing)
	      elv (vlax-get-property vp 'Elevation)
	      )
	(setq srfz (vlax-invoke-method vsrf 'FindElevationAtXY x y))
	if 
	 (= jedn 1)
		(progn
		(setq depth (rtos (* (- elv srfz) jedn) 2 3))
		);progn
	if
	 (> jedn 1)
		(progn
		(setq depth (rtos (* (- elv srfz) jedn) 2 0))
		);progn
	(setq elvx (rtos elv 2 2)
	      srfzx (rtos srfz 2 2)
	);setq
	(setq txtpos (list x y))
	(setq dev (strcat depth))
	(command "_text" txtpos txtH "0" dev)
	) 
     )
    (setq ctr (1+ ctr))
    )
  (princ)
  )

  (prompt "\nDFMA")