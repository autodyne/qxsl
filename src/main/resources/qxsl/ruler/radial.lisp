;; RADIAL LIBRARY DEFINED by 無線部開発班

(load "qxsl/ruler/common.lisp")


; imports
(import java.util.List)

(import java.time.ZoneId)
(import java.time.ZonedDateTime)

(import qxsl.extra.field.Band)
(import qxsl.extra.field.Call)
(import qxsl.extra.field.City)
(import qxsl.extra.field.Code)
(import qxsl.extra.field.Mode)
(import qxsl.extra.field.Name)
(import qxsl.extra.field.Note)
(import qxsl.extra.field.RSTQ)
(import qxsl.extra.field.Time)
(import qxsl.extra.field.Watt)


; contest
(defmacro CONTEST (var name scoring)
	`(setq ,var (contest ,name ,scoring)))


; section
(defmacro SECTION (contest name code test)
	`(. add ,contest ((section ,name ,code ,test))))


; hour
(defun hour (zoned zoneId)
	((access ZonedDateTime 'getHour)
		((access ZonedDateTime 'withZoneSameInstant)
			zoned ((access ZoneId 'of) null zoneId))))


; city
(defun city (dbname code level)
	(let (city ((access City 'forCode) null dbname code))
		(if (nil? level)
			city
			((access List 'get)
				((access City 'getFullPath) city) (integer level)))))
