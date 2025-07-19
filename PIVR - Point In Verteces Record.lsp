(defun c:PIVR (/ ss  NR NRr NP PD B old_cmdecho)
(setq old_cmdecho (getvar "cmdecho"))
(setvar "cmdecho" 0)
(initget (+ 1 2 4))
(setq NR (getint "\nPodaj numer pierwszego punktu:"))
(setq B (getreal "\Podaj wysokosc textu:"))
(setq NP (getfiled "Wpisz nazwe pliku" "" "txt;csv;xls" 1))
(SETQ PD (open NP "W"))
(command "_PDmode" 35)
(command "_PDsize" (/ B 2))
(setq NRr (- NR 1))
 (and
   (setq ss (ssget '((0 . "LINE,SPLINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE"))))
   ((lambda (i / l lst)
      (while 
	  (setq e (ssname ss (setq i (1+ i))))
        (foreach x (entget e)
          (and (eq 10 (car x))
               (not (member (setq l (list (cadr x) (caddr x) (cdr (assoc 38 (entget e))))) lst))
               (setq lst (cons l lst))
					  (setq WSPOLRZEDNA_X (RTOS (car l) 2 4))
					  (setq WSPOLRZEDNA_Y (RTOS (cadr l) 2 4))
					  (setq LINIA
						(strcat (rtos NR 2 0) " " WSPOLRZEDNA_Y " " WSPOLRZEDNA_X );strcat
					  );setq
					  (setq NR (1+ NR))
					  (setq NRr (1+ NRr))
	(WRITE-line LINIA PD)	      
		(progn	
			(DRAW_TEXT)
			(DRAW_PUNKT)
		);progn	
    (close PD)
    (setvar "cmdecho" old_cmdecho)
    (princ "\nKoniec")
    (princ)
            )
          
        )
      )
    )
     -1
   )
 );and
 );defun
(defun DRAW_TEXT (/)
  	 (command "_layer" "_m" "!GDX_nxyz-nr" "_c" "7" "" "")
  	 (command "_text" l B "0" NRr)
  );defun
;;; ---------------------------------------------------------------------------------- ;;;
(defun DRAW_PUNKT (/)
      (command "_layer" "_m" "!GDX_nxyz-pk" "_c" "7" "" "")
	  (command "_point" l)
  );defun
;;; ---------------------------------------------------------------------------------- ;;;
 (princ)