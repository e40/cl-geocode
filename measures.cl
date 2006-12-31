;; $Id$

(defpackage :util.measures
  (:use #:common-lisp #:excl)
  (:export #:location
	   #:location-latitude
	   #:location-longitude
	   #:make-location))

(provide :measures)

(in-package :util.measures)

(defstruct location latitude longitude)

(defmethod print-object ((location location) stream)
  (if* *print-escape*
     then (format stream "#<location ~a,~a>"
		  (location-latitude location)
		  (location-longitude location))
     else (format stream "~a,~a"
		  (location-latitude location)
		  (location-longitude location))))

(defmethod make-load-form ((self location) &optional environment)
  (make-load-form-saving-slots self :environment environment))
