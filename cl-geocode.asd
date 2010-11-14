
(in-package :cl-user)

(defclass my-cl-source-file (asdf:cl-source-file) ())

(defmethod asdf:source-file-type ((f my-cl-source-file) (m asdf:module))
  (declare (ignorable f m))
  "cl")

(asdf:disable-output-translations)

(asdf:defsystem cl-geocode
    :default-component-class cl-user::my-cl-source-file
    :components ((:file "package")
		 (:file "zip-util")
		 (:file "geocode" :depends-on ("package" "zip-util"))
		 ))
