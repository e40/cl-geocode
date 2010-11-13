;; This software is Copyright (c) Kevin Layer, 2006-2010.
;; You are granted the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :cl-geocode)

;; not thrilled with this name:
(defun location-near-p (location reference within)
  "Is LOCATION near REFERENCE location to WITHIN decimal degrees in both
latitude and longitude?"
  (and (< (abs (- (location-latitude location)
		  (location-latitude reference)))
	  within)
       (< (abs (- (location-longitude location)
		  (location-longitude reference)))
	  within)))

(defun distance-between (location1 location2 &key (unit :miles))
  "Calculate the straight line distance on the surface of the earth between
LOCATION1 and LOCATION2 using the \"Great Circle Distance Formula\".  The
values of the keyword UNIT can be :miles (the default), :nautical-miles or
:kilometers."
  ;; r * acos[sin(lat1) * sin(lat2) +
  ;;     cos(lat1) * cos(lat2) * cos(lon2 - lon1)]
  ;; Where r is the radius of the earth in whatever units you desire:
  ;;     r=3437.74677 (nautical miles)
  ;;     r=6378.7 (kilometers)
  ;;     r=3963.0 (statute miles)
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

;; The first thing we want to do is to define a special that contains the
;; zipcode data.  Because of the nature of data, because it contains
;; latitude and longitudes of the zipcodes, a likely use of this data will
;; be for searching.  That's why a simple vector was chosen.
;;
;; It would be simple matter to create, at compile time, a simple vector of
;; zipcode structs.  (This is so the zips.csv file is not needed at load time.)
;; It turns out that because the compiler must do circularity detection and
;; lots of other work, having the compiler write out a simple vector of
;; 33000+ struct objects is a very slow process, one that creates a lot of
;; garbage and uses lots of memory.  So, we use an alternate implementation
;; to read in the zips.csv file so the data can be constructed at load time.
;;
;; The technique used:
;; 
;; At compile time read the zips.csv file, creating a (temporary) simple
;; vector of zipcode structs.  Then, write the 5 slots of each zipcode struct
;; into 5 different vectors.  Then, at load time, we create *zipcodes*.
;;
;; It turns out the implementation below is about 10x more efficient
;; in time and more than 10x more efficient in space, than the
;; simple-vector or structs written out by the compiler.

(defvar *zipcodes*
    (destructuring-bind (codes abbrevs locations cities states)
;;;; Don't at read-time during the compile so the data file, zips.cvs, does
;;;; not need to be around to load the files.
	#.(let ((zipcodes
	       (prog2
		   (progn (format t "reading zips.cvs...")
			  (force-output))
		   (read-zipcodes-csv 
		    (merge-pathnames "zips.csv" *compile-file-pathname*))
		 (format t "done~%"))))
	  (list 'list
		(vector-of zipcodes #'zipcode-code)
		(vector-of zipcodes #'zipcode-state-abbrev)
		(vector-of zipcodes (lambda (zipcode)
				      (let ((loc (zipcode-location zipcode)))
					(cons (location-latitude loc)
					      (location-longitude loc)))))
		(vector-of zipcodes #'zipcode-city)
		(vector-of zipcodes #'zipcode-state)))
	(do* ((max (length codes))
	      (zipcodes (make-array max))
	      (i 0 (1+ i)))
	    ((= i max) zipcodes)
	  (setf (svref zipcodes i)
	    (make-zipcode :code (svref codes i)
			  :state-abbrev (svref abbrevs i)
			  :location (let ((loc (svref locations i)))
				      (make-location
				       :latitude (car loc)
				       :longitude (cdr loc)))
			  :city (svref cities i)
			  :state (svref states i))))))

(defun location-to-zipcode (location)
  ;; It turns out that finding the nearest zip for a given location is a
  ;; little tricky.  You can't just do a regular binary search, because
  ;; there are two parameters you are minimizing: latitude and longitude.
  ;; The follow solves the problem by brute force, traverse the entire
  ;; *zipcodes* array looking for the minimum distance between the target
  ;; location and each location in the vector.
  ;;
  (declare (optimize speed))
  (do* ((rlat (location-latitude location))
	(rlon (location-longitude location))
	(i 0 (1+ i))
	(zipcodes *zipcodes*)
	(max (length zipcodes))
	(best-i 0)
	(min-diff 10000.0)
	(lat 0.0)
	(lon 0.0)
	(diff 0.0)
	location zipcode)
      ((= i max) (svref zipcodes best-i))
    (declare (single-float rlat rlon min-diff lat lon diff)
	     (fixnum i max best-i))
    (setq zipcode (svref zipcodes i))
    (setq location (zipcode-location zipcode))
    (setq lat (location-latitude location))
    (setq lon (location-longitude location))
    (setq diff
      (the single-float
	(+ (the single-float (abs (the single-float (- lat rlat))))
	   (the single-float (abs (the single-float (- lon rlon)))))))
    (when (< diff min-diff)
      (setq best-i i)
      (setq min-diff diff))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See 
;; http://www.google.com/apis/maps/documentation/#Geocoding_HTTP_Request

(defvar *default-key* nil
  "A default Google Maps API key for calls to geocode.  You can obtain a
key by visiting http://www.google.com/apis/maps/signup.html.")

(defun geocode (&key q (key *default-key*) (output :csv))
  "Geocoding is the process of converting addresses (like \"1600
Amphitheatre Parkway, Mountain View, CA\") into geographic coodinates (like
latitude 37.423021 and longitude -122.083739), which you can use to place
markers or position the map based on street addresses in your database or
addresses supplied by users. The Google Maps API includes a geocoder that
can be accessed via HTTP request."
  (when (null key)
    (error "You must specify a Google Maps API key, which you can ~
obtain from the http://www.google.com/apis/maps/signup.html."))
  (when (null q)
    (error "You must specify an address to geocode (e.g., \"Oakland, CA\")."))
  (setq output
    (case output
      ((:csv :xml :kml :json) (string-downcase (symbol-name output)))
      (t (error "Bad :output keyword value: ~s." output))))
  (let ((query (query-to-form-urlencoded
		`(("q" . ,q) ("output" . ,output) ("key" . ,key))))
	(url-base "http://maps.google.com/maps/geo"))
    (values
     (do-http-request (format nil "~a?~a" url-base query)
       :method :get
       ;; Dunno if this is needed.  I started getting "connection reset by
       ;; peer" for a while, so I added this.  About the time I added it I
       ;; stopped getting them.  Hmmmmmmm.
       :user-agent "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-GB; rv:1.8.0.1) Gecko/20060111 Firefox/1.5.0.9"))))

(defun place-to-location (place &key (key *default-key*))
  (let ((result (geocode :q place :key key :output :csv)))
    ;; Using :csv, a good result starts with "200," and a bad result with
    ;; "602,".
    (when (and result (stringp result))
      (multiple-value-bind (found whole lat lon)
	  (match-re "200,[^,]+,(-?[0-9.]+),(-?[0-9.]+)" result)
	(declare (ignore whole))
	(when found
	  (make-location
	   :latitude (read-from-string lat)
	   :longitude (read-from-string lon)))))))

(defun location-to-place (location)
  (let ((zipcode (location-to-zipcode location)))
    (when zipcode
      (format nil "~@[~a, ~]~a"
	      (zipcode-city zipcode)
	      (zipcode-state zipcode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide :cl-geocode)
