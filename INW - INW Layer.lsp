(defun c:INW ( / ) ;INW Layer 
(command "_layer" "_m" "!GDX_INW-PKT" "_c" "7" """")
(command "_layer" "_m" "!GDX_INW-Studnie" "_c" "30" """")
(command "_layer" "_m" "!GDX_INW-Rzedna" "_c" "94" """")
(command "_layer" "_m" "!GDX_INW-OdchWysokosc" "_c" "1" """")
(command "_layer" "_m" "!GDX_OdchSytuacja_XY" "_c" "7" """")
(command "_layer" "_m" "!GDX_INW-Odleglosc" "_c" "6" """")
(command "_layer" "_m" "!GDX_INW-Sprzedane" "_c" "252" """")
(command "_layer" "_m" "!GDX_INW-Trasa" "_c" "7" """")
(princ)
);defun