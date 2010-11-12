;; This software is Copyright (c) Kevin Layer, 2006-2007.
;; You are granted the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This file contains utilities used only at compile time.

(in-package :cl-geocode)

(defun read-zipcodes-csv (csv-file)
  ;; Read the CSV file CSV-FILE which contains zip code data.  Return a
  ;; vector of `zip' structs, sorted by longitude then latitude.
  (flet ((string-to-number (string)
	   ;; Can't use parse-integer because these are floats.
	   (or (ignore-errors (read-from-string string))
	       (error "bad number: ~a." string))))
    (with-open-file (s csv-file)
      (let (line zips (nzips 0))
	(setq line (read-line s nil s))
	(when (eq line s) (error "bad zips file"))
	(loop
	  (setq line (read-line s nil s))
	  (when (eq line s) (return))
	  (destructuring-bind (zipcode state-abbrev lat lon city state)
	      (mapcar #'read-from-string (delimited-string-to-list line #\,))
	    (incf nzips)
	    (push (make-zipcode :code zipcode
				:state-abbrev state-abbrev
				:location (make-location
					   :latitude (string-to-number lat)
					   :longitude (string-to-number lon))
				:city (if* (= 0 (length city))
					 then nil
					 else city)
				:state state)
		  zips)))
	(setq zips (make-array nzips :initial-contents zips))

	;; Sort on longitude, then latitude
	(sort zips
	      (lambda (z1 z2)
		(if* (= (location-longitude (zipcode-location z1))
			(location-longitude (zipcode-location z2)))
		   then (< (location-latitude (zipcode-location z1))
			   (location-latitude (zipcode-location z2)))
		   else (< (location-longitude (zipcode-location z1))
			   (location-longitude (zipcode-location z2))))))))))

(defun vector-of (vector function)
  ;; Return a vector based on the contents of VECTOR created by calling
  ;; FUNCTION on each element of VECTOR.
  (let* ((len (length vector))
	 (res (make-array (length vector))))
    (dotimes (i len)
      (setf (svref res i) (funcall function (svref vector i))))
    res))
