
(defsystem cl-geocode
    :default-component-class cl-user::my-cl-source-file
    :components ((:file "package")
		 (:file "zip-util")
		 (:file "geocode" :depends-on ("package" "zip-util"))
		 ))
