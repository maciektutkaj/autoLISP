(defun c:SD ( / di len lendi en gr l1 l2 nm p1 p2 txtH txt txtpos p11 p1x p1y p2x p2y distp1p2 jedn)
	(command "_layer" "_m" "!GDX_OdchSytuacja" "_c" "7" """")
	(setq len (getreal "\nPodaj wielkosc: "))
	(setq txtH (getreal "\nPodaj wysokosc tekstu: "))
	(setq jedn (getreal "\nWybierz jednostki: \n[m] = 1; [cm] = 100; [mm] = 1000 "))
	(princ)
	(while
		(if
			(and
				(setq p1 (getpoint "\nWskaz punkt pomierzony: "))
				(setq p2 (getpoint "\nWskaz punkt projektowy: " p1))
			);and (warunek wskazania dwóch punktów)
	(progn
			(setq di (/ (distance p1 p2) 3.0); położenie podstawy groty na lini pomiędzy punktami
			lendi (/ len 3.0)
			nm (trans '(0. 0. 1.) 1 0 t)
			);setq
		(setq en
			(entget
				(entmakex
					(append
						(list
							'(0 . "LWPOLYLINE")
							'(100 . "AcDbEntity")
							'(100 . "AcDbPolyline")
							'(90 . 3)
							'(70 . 0)
						);list
						(setq l1
							(list
								(cons 10 (trans (polar p2 (angle p2 p1) len) 1 nm))
								(cons 10 (trans (polar p2 (angle p2 p1) lendi) 1 nm))
								(cons 40 (/ len 3.0)) ;szerokość podstawy grotu
								'(41 . 0.0)
								(cons 10 (trans p2 1 nm))
								(cons 210 nm)
							);list
						);setq
					);append
				);entmakex
			);entget
		);setq
	(setq l2
		(list
			(cons 10 (trans (polar p2 (angle p2 p1) len) 1 nm))
			'(40 . 0.0)
			(cons 41 (/ len 3.0))
			(cons 10 (trans (polar (polar p2 (angle p2 p1) len) (angle p1 p2) lendi) 1 nm))
			(cons 10 (trans p2 1 nm))
			(cons 210 nm)
		);list
	);setq
;---------------------------------------------------------------------------------------;
	;Wybór kierunku strzałki
	(setq p11 (trans (polar p2 (angle p2 p1) len) 1 nm))
	(setq en (reverse (member (assoc 39 en) (reverse en))))
		(princ "\nWskaz kierunek odchylki...")
			(while (= 5 (car (setq gr (grread t 13 0))))
				(entmod
					(append en
						(if (< (distance (cadr gr) p2) (distance (cadr gr) p11)) l1 l2)
					);append
				);entmod
			);while
	);progn
);if
; --------------------------------------------------------------------------------------;
(setq txtpos (getpoint "\nWskaz lokalizacje odchylki: "))
(setq p1x (car p1))
(setq p1y (cadr p1))
(setq p2x (car p2))
(setq p2y (cadr p2))
(setq distp1p2 (sqrt (+ (expt (- p2x p1x) 2) (expt (- p2y p1y) 2))))
(setq txt (strcat (rtos (* distp1p2 jedn) 2 0)))
(command "_text" txtpos txtH "0" txt)
);while
(princ)
)