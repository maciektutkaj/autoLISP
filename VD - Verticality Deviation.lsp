(defun c:VD ( / k1 k2 p1 p3 p4 p2 p1x p1y p2x p2y p3x p3y p4x p4y di nm en em l1 l2 len lendi odchX odchY txtH txt1 txt2 txtpos1 txtpos2 jedn alfa beta c c2)
(command "_layer" "_m" "!GDX_OdchSytuacja_XY" "_c" "7" """")
	(setq len (getreal "\nPodaj wielkosc: "))
	(setq txtH (getreal "\nPodaj wielkosc tekstu: "))
	(setq jedn (getreal "\nWybierz jednostki: \n[m] = 1; [cm] = 100; [mm] = 1000 "))
	(setq k1 (getpoint "\nOkresl kierunek, wybierz pierwszy punkt: "))
	(setq k2 (getpoint "\nOkresl kierunek, wybierz drugi punkt: "))	
(while
;---------------------------------------; strona północ-południe	
	(setq p1 (getpoint "\nWskaz punkt dolu NS: "))
	(setq p2 (getpoint "\nWskaz punkt gory NS: "))
	(setq p1x (car p1))
	(setq p2x (car p2))
	(setq p1y (cadr p1))
	(setq p2y (cadr p2))
	(setq nm (trans '(0. 0. 1.) 1 0 t))
	(setq di (/ (distance p1 p2) 3.0))
	(setq lendi (/ len 3.0))
	(princ)
;-------------------------------------------------; strzałka północ-południe
				(if (<= p1y p2y);północ
				(progn
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
										(cons 10 (trans (polar p2 (angle k1 k2) len) 1 nm))
										'(40 . 0.0)
										(cons 41 (/ len 3.0))
										(cons 10 (trans (polar p2 (angle k1 k2) (- len lendi)) 1 nm))
										(cons 10 (trans p2 1 nm))
										(cons 210 nm)
									);list
								);setq
							);append
						);entmakex
					);entget
				);setq
					(setq txtpos1 (list  (+ (car p2) (/ len 3))(+ (cadr p2) len)))
				);progn
				);if
				(if (> p1y p2y); południe
				(progn
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
										(cons 10 (trans (polar p2 (+ (angle k1 k2) pi) len) 1 nm))
										'(40 . 0.0)
										(cons 41 (/ len 3.0))
										(cons 10 (trans (polar p2 (+ (angle k1 k2
										) pi) (- len lendi)) 1 nm))
										(cons 10 (trans p2 1 nm))
										(cons 210 nm)
									);list
								);setq
							);append
						);entmakex
					);entget
				);setq
					(setq txtpos1 (list  (+ (car p2) (/ len 3))(- (cadr p2) len)))
				);progn
				);if
					;-------------------------------------;	ODCHYŁKA 
	(setq alfa (- (angle k1 k2) (angle p2 p1)))
	(setq c (sqrt (+ (expt (- p2x p1x) 2) (expt (- p2y p1y) 2)))) 
	(setq odchX (abs (* (cos alfa) c)))
	;-------------------------------------;
;---------------------------------------; storna wschód-zachód				
	(setq p3 (getpoint "\nWskaz punkt dolu EW: "))
	(setq p4 (getpoint "\nWskaz punkt gory EW: "))
	(setq p3x (car p3))
	(setq p4x (car p4))
	(setq p3y (cadr p3))
	(setq p4y (cadr p4))
	(princ)
;---------------------------------------; strzałka wschód-zachód				
				(if (<= p3x p4x);wschód
				(progn
				(setq em
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
								(setq l2
									(list
										(cons 10 (trans (polar p4 (+ (angle k2 k1) (* pi 0.5)) len) 1 nm))
										'(40 . 0.0)
										(cons 41 (/ len 3.0))
										(cons 10 (trans (polar p4 (+ (angle k2 k1) (* pi 0.5)) (- len lendi)) 1 nm))
										(cons 10 (trans p4 1 nm))
										(cons 210 nm)
									);list
								);setq
							);append
						);entmakex
					);entget
				);setq
				(setq txtpos2 (list  (+ (car p4) len)(- (cadr p4) (/ len 3))))
				);progn
				);if
				(if (> p3x p4x);zachód
				(progn
				(setq em
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
								(setq l2
									(list
										(cons 10 (trans (polar p4 (+ (angle k2 k1) (* pi 1.5)) len) 1 nm))
										'(40 . 0.0)
										(cons 41 (/ len 3.0))
										(cons 10 (trans (polar p4 (+ (angle k2 k1) (* pi 1.5)) (- len lendi)) 1 nm))
										(cons 10 (trans p4 1 nm))
										(cons 210 nm)
									);list
								);setq
							);append
						);entmakex
					);entget
				);setq
				(setq txtpos2 (list  (- (car p4) len)(+ (cadr p4) (/ len 3))))
				);progn
				);if
;-------------------------------------; TEXT
	;-------------------------------------;	ODCHYŁKA 
	(setq beta (- (angle k1 k2) (angle p4 p3)))
	(setq c2 (sqrt (+ (expt (- p4x p3x) 2) (expt (- p4y p3y) 2)))) 
	(setq odchY (abs (* (sin beta) c2)))
	;-------------------------------------;
	
	(setq txt1
	(entmake
		(list
			'(0 . "TEXT")
			'(100 . "AcDbText")
			(cons 10 txtpos1)
			(cons 40 txtH)
			(cons 1 (rtos (* odchX jedn) 2 0))
		);list
	);entmake
);setq
(setq txt2
	(entmake
		(list
			'(0 . "TEXT")
			'(100 . "AcDbText")
			(cons 10 txtpos2)
			(cons 40 txtH)
			(cons 1 (rtos (* odchY jedn) 2 0))
		);list
	);entmake
);setq
);while
(princ)
);defun