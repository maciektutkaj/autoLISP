
(defun c:NOD(/ NR B txt WSKAZ_PUNKT)
; NR- numer punktu
; B- wyskokość tekstu
    (setq NR (getint "\nPodaj numer pierwszego punktu:"))
    (setq B (getreal "\Podaj wysokosc textu:"))
        (while
	  (setq WSKAZ_PUNKT (getpoint "\nWskaz punkt tekstu"))
		(command "_layer" "_m" "!GDX_NOD" "_c" "7" "" "")
			(setq txt
				(entmake
					(list
						'(0 . "TEXT")
						'(100 . "AcDbText")
						(cons 10 WSKAZ_PUNKT)
						(cons 40 B)
						(cons 1 (rtos NR 2 0))
					);list
				);entmake
			);setq
	  (setq NR (1+ NR))
	);WHILE
    (princ "\nKoniec")
    (princ)
);defun
(prompt "\nNOD")
