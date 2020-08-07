;; RADIAL LIBRARY DEFINED by 無線部開発班

(load "qxsl/ruler/format.lisp")


; imports
(import java.util.List)

(import java.time.ZoneId)
(import java.time.ZonedDateTime)

(import qxsl.ruler.Contest)
(import qxsl.ruler.Section)

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
(defmacro SECTION (var name code test)
	`((method 'add Contest Section)
		,var (section ,name ,code ,test)))


; hour
(defun hour (zoned zoneId)
	((method 'getHour ZonedDateTime)
		((method 'withZoneSameInstant ZonedDateTime ZoneId)
			zoned ((method 'of ZoneId String) null zoneId))))


; city
(defun city (dbname code level)
	(let city ((method 'forCode City String String) null dbname code)
		(if (nil? level)
			city
			((method 'get List int)
				((method 'getFullPath City) city) (integer level)))))
