(defun c:SDA( / di len lendi en gr l1 l2 nm p1 p2 txtH txt txtpos p11 p1x p1y p2x p2y distp1p2 jedn sobject)
	(command "_layer" "_m" "!GDX_OdchSytuacja" "_c" "7" """")
	(setq len (getreal "\nPodaj wielkosc: "))
	(setq txtH (getreal "\nPodaj wysokosc tekstu: "))
	(setq jedn (getreal "\nWybierz jednostki: \n[m] = 1; [cm] = 100; [mm] = 1000 "))
	(princ)
	(while
		(setq p1 (getpoint "\nWskaz punkt pomierzony: "))
		(setq sobject (car (entsel "\nWybierz objekt projektowy: ")))
           (member (cdr (assoc 0 (entget sobject)))
                   '("LWPOLYLINE" "POLYLINE" "LINE" "3dPOLYLINE"))
		(setq p2 (vlax-curve-getClosestPointTo sobject p1 t))
	(progn
			(setq di (/ len 3.0); położenie podstawy grotu na lini pomiędzy punktami
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
								'(40 . 0.0)
								(cons 41 (/ len 3.0))
								(cons 10 (trans (polar (polar p2 (angle p2 p1) len) (angle p1 p2) lendi) 1 nm))
								(cons 10 (trans p2 1 nm))
								(cons 210 nm)
							);list
						);setq
					);append
				);entmakex
			);entget
		);setq
	);progn
; --------------------------------------------------------------------------------------;
(if 
	(and (> (car p1) (car p2)) (> (cadr p1) (cadr p2)))
		(progn
			(setq txtpos (list (+ (car (trans (polar p2 (angle p2 p1) len) 1 nm)) (* txtH 0.3))(+ (cadr (trans (polar p2 (angle p2 p1) len) 1 nm)) (* txtH 0.3))))
		);progn
);if
(if 
	(and (<= (car p1) (car p2)) (>= (cadr p1) (cadr p2)))
		(progn
			(setq txtpos (list (- (car (trans (polar p2 (angle p2 p1) len) 1 nm)) (+ txtH (* txtH 0.3)))(+ (cadr (trans (polar p2 (angle p2 p1) len) 1 nm)) (* txtH 0.3))))
		);progn
);if
(if 
	(and (< (car p1) (car p2)) (< (cadr p1) (cadr p2)))
		(progn
			(setq txtpos (list (- (car (trans (polar p2 (angle p2 p1) len) 1 nm)) (+ txtH (* txtH 0.3)))(- (cadr (trans (polar p2 (angle p2 p1) len) 1 nm)) (+ txtH (* txtH 0.3)))))
		);progn
);if
(if 
	(and (>= (car p1) (car p2)) (<= (cadr p1) (cadr p2)))
		(progn
			(setq txtpos (list (+ (car (trans (polar p2 (angle p2 p1) len) 1 nm)) (* txtH 0.3))(- (cadr (trans (polar p2 (angle p2 p1) len) 1 nm)) (* txtH 0.3))))
		);progn
);if
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
(prompt "\nSDA")
