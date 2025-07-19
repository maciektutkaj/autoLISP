(defun C:CRD(/ len txtH jedn k1 k2 fi fi2 st nm ptss n len pt1 pt2 xW yW ptW p1 p2 p1x p1y p2x p2y di en em l1 l2 len lendi odchX odchY txtH txt1 txt2 txtpos1 txtpos2 jedn alfa c); = CENTER REINFORCMENT DEVIATION
	(setq fi (getreal "\nPodaj srednice wytyku (mm): "))
	(setq fi2 (* (* fi 0.5) 0.001))
	(setq st (getpoint "\nWskaz punkt stanowiska: "))
	(setq nm (trans '(0. 0. 1.) 1 0 t))
	(setq len (getreal "\nPodaj wielkosc: "))
	(setq txtH len)
	(setq jedn 1000)
	(setq k1 (getpoint "\nOkresl kierunek, wybierz pierwszy punkt: "))
	(setq k2 (getpoint "\nOkresl kierunek, wybierz drugi punkt: "))
	(while
		(progn
		(command "_layer" "_m" "!GDX_Wytyk" "_c" "50" """")
			(setq pt1 (getpoint "\nWskaz pomierzony punkt: "))
			(setq pt2 (trans (polar st (angle st pt1) (+ (distance st pt1) fi2)) 1 nm))
			(setq xW (car pt2))
			(setq yW (cadr pt2))
			(setq ptW (list xW yW))
			(progn
				(command "_layer" "_m" "!GDX_OdchSytuacja_XY" "_c" "7" """")
					(setq p1 (getpoint "\nWskaz punkt projektowy: "))
					(setq p2 ptW)
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
				;---------------------------------------; strzałka wschód-zachód				
								(if (<= p1x p2x);wschód
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
														(cons 10 (trans (polar p2 (+ (angle k2 k1) (* pi 0.5)) len) 1 nm))
														'(40 . 0.0)
														(cons 41 (/ len 3.0))
														(cons 10 (trans (polar p2 (+ (angle k2 k1) (* pi 0.5)) (- len lendi)) 1 nm))
														(cons 10 (trans p2 1 nm))
														(cons 210 nm)
													);list
												);setq
											);append
										);entmakex
									);entget
								);setq
								(setq txtpos2 (list  (+ (car p2) len)(- (cadr p2) (/ len 3))))
								);progn
								);if
								(if (> p1x p2x);zachód
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
														(cons 10 (trans (polar p2 (+ (angle k2 k1) (* pi 1.5)) len) 1 nm))
														'(40 . 0.0)
														(cons 41 (/ len 3.0))
														(cons 10 (trans (polar p2 (+ (angle k2 k1) (* pi 1.5)) (- len lendi)) 1 nm))
														(cons 10 (trans p2 1 nm))
														(cons 210 nm)
													);list
												);setq
											);append
										);entmakex
									);entget
								);setq
								(setq txtpos2 (list  (- (car p2) len)(+ (cadr p2) (/ len 3))))
								);progn
								);if
				;-------------------------------------; TEXT
					;-------------------------------------;	ODCHYŁKA 
					(setq alfa (- (angle k1 k2) (angle p2 p1)))
					(setq c (sqrt (+ (expt (- p2x p1x) 2) (expt (- p2y p1y) 2)))) 
					(setq odchX (abs (* (sin alfa) c)))
					(setq odchY (abs (* (cos alfa) c)))
					;-------------------------------------;
					
					(setq txt1
					(entmake
						(list
							'(0 . "TEXT")
							'(100 . "AcDbText")
							(cons 10 txtpos2)
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
							(cons 10 txtpos1)
							(cons 40 txtH)
							(cons 1 (rtos (* odchY jedn) 2 0))
						);list
					);entmake
				);setq
			);progn
		);progn
	);while
 );defun

(prompt "\nCRD")