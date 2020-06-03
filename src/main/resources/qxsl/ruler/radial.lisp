;; RADIAL LIBRARY DEFINED by 無線部開発班

(load "qxsl/ruler/common.lisp")

; imports
(import java.util.List)

(import java.time.Month)
(import java.time.LocalDate)
(import java.time.ZonedDateTime)
(import java.time.temporal.TemporalAdjusters)

(import qxsl.extra.field.City)


; hour
(defun hour (zoned zoneId)
	((access ZonedDateTime 'getHour)
		((access ZonedDateTime 'withZoneSameInstant)
			zoned ((access java.time.ZoneId 'of) null zoneId))))


; city
(defun city (dbname code level)
	(let (city ((access City 'forCode) null dbname code))
		(if (nil? level)
			city
			((access List 'get)
				((access City 'getFullPath) city) (integer level)))))


; date
(defun date (year month dayOfWeek nth)
	((access LocalDate 'with)
		((access LocalDate 'of) year ((access Month 'valueOf) month) 1)
		((access TemporalAdjusters 'dayOfWeekInMonth) nth dayOfWeek)))
