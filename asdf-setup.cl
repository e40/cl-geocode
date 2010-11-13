
(defclass my-cl-source-file (asdf:cl-source-file) ())

(defmethod asdf:source-file-type ((f my-cl-source-file) (m asdf:module))
  (declare (ignorable f m))
  "cl")

(asdf:disable-output-translations)
