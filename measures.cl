;; $Id$

(defpackage :util.measures
  (:use #:common-lisp #:excl)
  (:export #:location
	   #:location-latitude
	   #:location-longitude
	   #:make-location
	   
	   #:distance-between
	   #:location-near-p))

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

;; not thrilled with this name:
(defun location-near-p (location reference within)
  (and (< (abs (- (location-latitude location)
		  (location-latitude reference)))
	  within)
       (< (abs (- (location-longitude location)
		  (location-longitude reference)))
	  within)))

(defun distance-between (location1 location2 &key (unit :miles))
  ;; Use the "Great Circle Distance Formula" (also called the Consine Law
  ;; formula?) to calculate the distance between two coordinates:
  ;;    r * acos[sin(lat1) * sin(lat2) +
  ;;        cos(lat1) * cos(lat2) * cos(lon2 - lon1)]
  ;; Where r is the radius of the earth in whatever units you desire:
  ;;   r=3437.74677 (nautical miles)
  ;;   r=6378.7 (kilometers)
  ;;   r=3963.0 (statute miles)
  ;; However, the values of degrees must be converted to radians before
  ;; passing them to the trig functions.
  ;;
  ;; A nice page to show this is here:
  ;;  http://www.movable-type.co.uk/scripts/LatLong.html
  (flet ((radians (deg) (* deg (/ pi 180.0))))
    (let ((lat1 (radians (location-latitude location1)))
	  (lat2 (radians (location-latitude location2)))
	  (lon1 (radians (location-longitude location1)))
	  (lon2 (radians (location-longitude location2)))
	  (radius (case unit
		    (:miles 3963.0)
		    (:nautical-miles 3437.74677)
		    (:kilometers 6378.7))))
      (* radius
	 (acos (+ (* (sin lat1) (sin lat2))
		  (* (cos lat1) (cos lat2) (cos (- lon2 lon1)))))))))
