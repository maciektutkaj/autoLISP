
(defun c:nxyz(/ NR NP ZN PD B SP old_cmdecho)
; NR- numer punktu
; NP- nazwa pliku
; ZN- znacznik
; PD- plik docelowy
; B- wyskokość tekstu
; SP- sprzęt
    (setq old_cmdecho (getvar "cmdecho"))
    (setvar "cmdecho" 0)
    (initget (+ 1 2 4)) 
	(setq SP (getstring "\Wybierz sprzet [T - tachimetr/ G - GPS]:"))
	(setq NP (getfiled "Wpisz nazwe pliku" "" "txt;csv;xls" 1))
    (setq NR (getint "\nPodaj numer pierwszego punktu:"))
    (setq B (getreal "\Podaj wysokosc textu:"))
    (setq ZN (getstring "\Czy wstawiac znacznik punktu [T/N]:"))
    (SETQ PD (open NP "W"))
	(command "_PDmode" 35)
	(command "_PDsize" (/ B 2))
        (while
	  (setq WSKAZ_PUNKT (getpoint "\nWskaz punkt"))
	  (setq WSPOLRZEDNA_X (RTOS (car WSKAZ_PUNKT) 2 4))
	  (setq WSPOLRZEDNA_Y (RTOS (cadr WSKAZ_PUNKT) 2 4))
	  (setq WSPOLRZEDNA_Z (RTOS (caDdr WSKAZ_PUNKT)2 4))
	  (setq LINIA
		(if (or (= SP "T")(= SP "t"))
			(progn
				(strcat (rtos NR 2 0) " " WSPOLRZEDNA_Y " " WSPOLRZEDNA_X " " WSPOLRZEDNA_Z );strcat
			);progn
				(strcat (rtos NR 2 0) "," WSPOLRZEDNA_Y "," WSPOLRZEDNA_X "," WSPOLRZEDNA_Z );strcat
		);if
	  );setq
    	  (WRITE-line LINIA PD)
			 (command "_layer" "_m" "!GDX_nxyz-nr" "_c" "7" "" "")
			 (command "_text" WSKAZ_PUNKT B "0" NR)
			 (command "_layer" "_m" "!GDX_nxyz-pk" "_c" "7" "" "")
			 (command "_point" WSKAZ_PUNKT)
	  (setq NR (1+ NR))
	);WHILE
    (close PD)
    (setvar "cmdecho" old_cmdecho)
    (princ "\nKoniec")
    (princ)
  );defun

(princ
  (strcat
    "\nPolecenie: nxyz "
  )
)
