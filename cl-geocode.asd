
(in-package :cl-user)

(defclass cl-geocode-source-file (asdf:cl-source-file) ())

(defmethod asdf:source-file-type ((f cl-geocode-source-file) (m asdf:module))
  (declare (ignorable f m))
  "cl")

(asdf:disable-output-translations)

(asdf:defsystem cl-geocode
    :default-component-class cl-geocode-source-file
    :components ((:file "package")
		 (:file "zip-util")
		 (:file "geocode" :depends-on ("package" "zip-util")))
    #+sbcl :depends-on
    #+sbcl ("aserve" "acl-compat" "cl-ppcre"))
