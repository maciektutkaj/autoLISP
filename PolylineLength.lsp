(vl-load-com)
(defun c:pol ( / zbior licz dlug elem)
(setq zbior (ssget '((0 . "*POLYLINE")))
licz 0
dlug 0)
(repeat (sslength zbior)
(setq elem (vlax-ename->vla-object (ssname zbior licz))
dlug (+ dlug (vla-get-Length elem))
licz (1+ licz)))
(alert (strcat "D³ugoœæ wskazanych elementów wynosi: " (rtos dlug 2 3))) ; 3 - liczba miejsc po przecinku
)
(prompt "\nKOMENDA -> POL") 