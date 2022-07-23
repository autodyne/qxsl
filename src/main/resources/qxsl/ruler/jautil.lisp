;; UTILITIES for JAPAN DEFINED by 無線部開発班

(load "qxsl/ruler/format.lisp")

; hour in JST
(defun 時刻 it (hour (at-zone (qxsl-time it) (zone "Asia/Tokyo"))))

; city bases
(setq CITY-BASE ((method 'load LocalCityBase String) "qxsl/local/city.ja"))
(setq AREA-BASE ((method 'load LocalCityBase String) "qxsl/local/area.ja"))

; city lists
(setq CITY-LIST ((method! 'toList) CITY-BASE))
(setq AREA-LIST ((method! 'toList) AREA-BASE))

; city extraction
(defun CITY it (city<-code CITY-BASE (qxsl-code it)))
(defun PREF ct (city<-name CITY-BASE (city-area ct)))
(defun AREA ct (city<-code AREA-BASE (city-code ct)))

; city validation
(defun 現存? it (not (null? (CITY it))))
(defun 諸島? it (equal (qxsl-code it) "48"))
(defun 支庁? it (match (qxsl-code it) "\\d{3,3}"))
(defun 市郡? it (match (qxsl-code it) "\\d{4,9}"))
(defun 府県? it (not (some it 支庁? 市郡? 諸島?)))

; city pattern matching
(defmacro city-match regex `(lambda ct (match (city-code ct) ,regex)))

; area validation for city
(defun city-area1? ct (equal "1" (city-area (AREA (PREF ct)))))
(defun city-area2? ct (equal "2" (city-area (AREA (PREF ct)))))
(defun city-area3? ct (equal "3" (city-area (AREA (PREF ct)))))
(defun city-area4? ct (equal "4" (city-area (AREA (PREF ct)))))
(defun city-area5? ct (equal "5" (city-area (AREA (PREF ct)))))
(defun city-area6? ct (equal "6" (city-area (AREA (PREF ct)))))
(defun city-area7? ct (equal "7" (city-area (AREA (PREF ct)))))
(defun city-area8? ct (equal "8" (city-area (AREA (PREF ct)))))
(defun city-area9? ct (equal "9" (city-area (AREA (PREF ct)))))
(defun city-area0? ct (equal "0" (city-area (AREA (PREF ct)))))

; cities in area
(setq cities-area1 (remove-if-not city-area1? CITY-LIST))
(setq cities-area2 (remove-if-not city-area2? CITY-LIST))
(setq cities-area3 (remove-if-not city-area3? CITY-LIST))
(setq cities-area4 (remove-if-not city-area4? CITY-LIST))
(setq cities-area5 (remove-if-not city-area5? CITY-LIST))
(setq cities-area6 (remove-if-not city-area6? CITY-LIST))
(setq cities-area7 (remove-if-not city-area7? CITY-LIST))
(setq cities-area8 (remove-if-not city-area8? CITY-LIST))
(setq cities-area9 (remove-if-not city-area9? CITY-LIST))
(setq cities-area0 (remove-if-not city-area0? CITY-LIST))

; cities outside of area
(setq out-of-area1 (remove-if city-area1? CITY-LIST))
(setq out-of-area2 (remove-if city-area2? CITY-LIST))
(setq out-of-area3 (remove-if city-area3? CITY-LIST))
(setq out-of-area4 (remove-if city-area4? CITY-LIST))
(setq out-of-area5 (remove-if city-area5? CITY-LIST))
(setq out-of-area6 (remove-if city-area6? CITY-LIST))
(setq out-of-area7 (remove-if city-area7? CITY-LIST))
(setq out-of-area8 (remove-if city-area8? CITY-LIST))
(setq out-of-area9 (remove-if city-area9? CITY-LIST))
(setq out-of-area0 (remove-if city-area0? CITY-LIST))

; area validation for item
(defun AREA1? it (member (CITY it) cities-area1))
(defun AREA2? it (member (CITY it) cities-area2))
(defun AREA3? it (member (CITY it) cities-area3))
(defun AREA4? it (member (CITY it) cities-area4))
(defun AREA5? it (member (CITY it) cities-area5))
(defun AREA6? it (member (CITY it) cities-area6))
(defun AREA7? it (member (CITY it) cities-area7))
(defun AREA8? it (member (CITY it) cities-area8))
(defun AREA9? it (member (CITY it) cities-area9))
(defun AREA0? it (member (CITY it) cities-area0))

; returns the environment
(pattern normalize transform)
