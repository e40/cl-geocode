
(require :measures) provides the :util.measures package.

A location is described by a latitude and longitude in decimal
degrees.

cl-user(2): (require :measures)
; Fast loading /usr/local/lisp/code/measures.fasl
t
cl-user(3): (use-package :util.measures)
t
cl-user(4): (setq location1
	      (make-location :latitude 38.005 :longitude -121.804726))
#<location 38.005,-121.804726>
cl-user(5): (setq location2
	      (make-location :latitude 37.824444 :longitude -122.23055))
#<location 37.824444,-122.23055>
cl-user(6): (distance-between location1 location2)
26.379927592149343d0
cl-user(7): (distance-between location1 location2 :unit :kilometers)
42.46016888326334d0
cl-user(8): 

;; The following examples inspired by
;;   http://nationalatlas.gov/articles/mapping/a_latlong.html#four

;; A degree of latitude should be approximately 69 miles:
cl-user(8): (distance-between
	     (make-location :latitude 38.0 :longitude -121.0)
	     (make-location :latitude 37.0 :longitude -121.0))
69.16739825655358d0
cl-user(9): 

;; A degree of longitude (at the equator) is also approximately 69
;; miles:
cl-user(9): (distance-between
	     (make-location :latitude 0.0 :longitude -121.0)
	     (make-location :latitude 0.0 :longitude -122.0))
69.16739825652837d0
cl-user(10): 

;; But as you move north or south the size gradually decreases to zero as
;; the meridians converge at the poles:

cl-user(10): (distance-between
	     (make-location :latitude 37.0 :longitude -121.0)
	     (make-location :latitude 37.0 :longitude -122.0))
55.23928644738535d0
cl-user(11): (distance-between
	     (make-location :latitude 50.0 :longitude -121.0)
	     (make-location :latitude 50.0 :longitude -122.0))
44.459615443471485d0
cl-user(12): (distance-between
	     (make-location :latitude 80.0 :longitude -121.0)
	     (make-location :latitude 80.0 :longitude -122.0))
12.010644812832272d0
cl-user(13): (distance-between
	     (make-location :latitude 90.0 :longitude -121.0)
	     (make-location :latitude 90.0 :longitude -122.0))
0.0d0
cl-user(14): 


Sometimes it is nice to see if a location is near another:

cl-user(15): (location-near-p
	      (make-location :latitude 37.824444 :longitude -122.23055)
	      (make-location :latitude 38.005 :longitude -121.804726)
	      1.0)
t
cl-user(16): (location-near-p
	      (make-location :latitude 37.824444 :longitude -122.23055)
	      (make-location :latitude 38.005 :longitude -121.804726)
	      0.5)
t
cl-user(17): (location-near-p
	      (make-location :latitude 37.824444 :longitude -122.23055)
	      (make-location :latitude 38.005 :longitude -121.804726)
	      0.1)
nil
cl-user(18): 
