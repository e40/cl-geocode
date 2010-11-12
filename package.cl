;; This software is Copyright (c) Kevin Layer, 2006-2010.
;; You are granted the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage :cl-geocode
  (:use #:common-lisp #+allegro #:excl)
  (:export #:location
	   #:location-latitude
	   #:location-longitude
	   #:make-location
	   
	   #:distance-between
	   #:location-near-p)
  
  (:export #:*zipcodes*
	   
	   #:zipcode-p
	   #:zipcode-code
	   #:zipcode-state-abbrev
	   #:zipcode-latitude
	   #:zipcode-longitude
	   #:zipcode-city
	   #:zipcode-state
	   
	   #:location-to-zipcode))

(in-package :cl-geocode)

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




(defstruct zipcode code state-abbrev location city state)

(defmethod print-object ((zipcode zipcode) stream)
  (format stream "#<zipcode ~a (~@[~a, ~]~a): ~a>"
	  (zipcode-code zipcode) (zipcode-city zipcode) (zipcode-state zipcode)
	  (zipcode-location zipcode)))

