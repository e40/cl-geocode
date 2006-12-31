
(distance-between '(38.005 . -121.804726)
		  '(37.824444 . -122.23055))
;; should be (approximately) 42.4 kilometers or 26.4 miles

;; Examples inspired by
;;   http://nationalatlas.gov/articles/mapping/a_latlong.html#four

;; Another test is that a degree of latitude is approximately 69 miles.
(distance-between '(38.0 . -121.0)
		  '(37.0 . -121.0))
;; 69.16739825655358d0

;; A degree of longitude (at the equator) is also approximately 69 miles.
cl-user(96): (distance-between '(0.0 . -121.0)
			       '(0.0 . -122.0))
69.16739825652837d0

;; But as you move north or south the size gradually decreases to zero as
;; the meridians converge at the poles:

cl-user(97): (distance-between '(37.0 . -121.0)
			       '(37.0 . -122.0))
55.23928644738535d0
cl-user(98): (distance-between '(39.0 . -121.0)
			       '(39.0 . -122.0))
53.752894011398546d0
cl-user(99): (distance-between '(80.0 . -121.0)
			       '(80.0 . -122.0))
12.010644812832272d0
cl-user(100): (distance-between '(90.0 . -121.0)
				'(90.0 . -122.0))
0.0d0
cl-user(101): 
