;copyright (c)2014 by Timothy Corey
;Delta Engineering Systems, Redding, CA
;Autodesk Authorized Value-Added Reseller
;permission is hereby granted for free use of this program as-is
;or modified to your specifications.
;This program or any portion may not be sold or combined with any
;for-sale software.
;It is the responsibility of the user to debug the software
;to determine its usability and accuracy.

;DFM - Deviation From Model report
(defun c:dfm ( / srf vsrf fl pts len ctr p vp pno x y elv srfz depth elvx srfzx output old_cmdecho NP)
   
   (setq old_cmdecho (getvar "cmdecho"))
    (setvar "cmdecho" 0)
    (initget (+ 1 2 4)) 
	(setq NP (getfiled "Wpisz nazwe pliku" "" "txt;csv;xls" 1))
    (setq fl (open NP "W"))
  (setq srf (car (entsel "\nSelect reference surface: "))
	vsrf (vlax-ename->vla-object srf)
	)
  (write-line "PointNumber, PointElevation, SurfaceElevation, DistanceToSurface" fl)
  (prompt "\nSelect points to report: ")
  
  
  (setq pts (ssget)
	len (sslength pts)
	ctr 0)

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
	(setq depth (rtos (- srfz elv) 2 2))
	(setq elvx (rtos elv 2 2)
	      srfzx (rtos srfz 2 2)
	      
	      )
	(setq output (strcat pno "," elvx "," srfzx "," depth))
	(write-line output fl)
	
	)
      
      )
    (setq ctr (1+ ctr))
    )
  (close fl)
  (princ)
  )
  (prompt "\nDFM")