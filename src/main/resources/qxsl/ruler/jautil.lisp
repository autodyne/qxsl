;; UTILITIES for JAPAN DEFINED by 無線部開発班

(load "qxsl/ruler/format.lisp")

; hour in JST
(defun 時刻 it (hour (at-zone (qxsl-time it) (zone "Asia/Tokyo"))))

; city databases
(setq CITYDB ((method 'load LocalCityBase String) "qxsl/local/city.ja"))
(setq AREADB ((method 'load LocalCityBase String) "qxsl/local/area.ja"))

; city extraction
(defun 市区町村 it (city<-code CITYDB (qxsl-code it)))
(defun 都道府県 it (city<-name CITYDB (area<-city (市区町村 it))))
(defun 所轄総通 it (city<-code AREADB (code<-city (都道府県 it))))

; city validation
(defun 現存? it (not (null? (市区町村 it))))
(defun 支庁? it (match (code<-city (市区町村 it)) "\\d{3,3}"))
(defun 市郡? it (match (code<-city (市区町村 it)) "\\d{4,9}"))
(defun 諸島? it (equal (code<-city (市区町村 it)) "48"))
(defun 府県? it (not (some it 支庁? 市郡? 諸島?)))

; area validation
(defun AREA1? it (equal (area<-city (所轄総通 it)) "1"))
(defun AREA2? it (equal (area<-city (所轄総通 it)) "2"))
(defun AREA3? it (equal (area<-city (所轄総通 it)) "3"))
(defun AREA4? it (equal (area<-city (所轄総通 it)) "4"))
(defun AREA5? it (equal (area<-city (所轄総通 it)) "5"))
(defun AREA6? it (equal (area<-city (所轄総通 it)) "6"))
(defun AREA7? it (equal (area<-city (所轄総通 it)) "7"))
(defun AREA8? it (equal (area<-city (所轄総通 it)) "8"))
(defun AREA9? it (equal (area<-city (所轄総通 it)) "9"))
(defun AREA0? it (equal (area<-city (所轄総通 it)) "0"))

; returns the environment as a pattern
(pattern normalize transform)
