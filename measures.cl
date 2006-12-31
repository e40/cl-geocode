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

