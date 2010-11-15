
(in-package :cl-user)

(asdf:load-system 'cl-geocode)

(use-package :cl-geocode)

(load "..//google-maps-key.cl")

(assert (string= "Berkeley, California"
		 (location-to-place
		  (place-to-location
		   "2629 College Ave, Berkeley, CA"))))

(exit 0)
