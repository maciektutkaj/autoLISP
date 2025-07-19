;; by sinc @ the Swamp 07/22/2004
;; Repeatedly draws a line from a pick point perpendicular
;; to a selected object
(defun c:perp2ent (/ entity pt)
(while (setq entity (car (entsel "\nSelect entity: ")))
(while
(setq pt (getpoint "\nSelect point to draw perpendicular from: "))
(entmake (list '(0 . "LINE") (cons 10 (trans pt 1 0))
(cons 11 (vlax-curve-getClosestPointTo entity (trans pt 1 0)))) ;_ list
) ;_ entmake
) ;_ while
) ;_ while
(princ)
) ;_ defun
(prompt "\nKOMENDA -> perp2ent") 