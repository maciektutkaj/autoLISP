(defun C:CCRm (/ fi fi2 st nm ptss n len pt1 pt2 xW yW ptW txt2); = Calculate Center of Reinforcment
(command "_layer" "_m" "!GDX_Wytyk" "_c" "50" """")
	(setq fi (getreal "\nPodaj srenice wytyku (mm): "))
	(setq fi2 (* (* fi 0.5) 0.001))
	(setq st (getpoint "\nWskaz punkt stanowiska: "))
	(setq nm (trans '(0. 0. 1.) 1 0 t))
	(while
		(progn
			(setq pt1 (getpoint "n\Wzkaz pomierzony punkt: "))
			(setq pt2 (trans (polar st (angle st pt1) (+ (distance st pt1) fi2)) 1 nm))
			(setq xW (car pt2))
			(setq yW (cadr pt2))
			(setq ptW (list xW yW))
			(setq txt2
				(entmake
					(list
						'(0 . "POINT")
						'(100 . "AcDbpOINT")
						(cons 10 ptW)
					);list
				);entmake
			);setq
		);progn
	);while
 )
(prompt "\nCCRm")