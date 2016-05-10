;;;; qelt.asd

(asdf:defsystem #:qelt
  :description "Describe qelt here"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:sketch
               #:easing)
  :serial t
  :components ((:file "package")
               (:file "qelt")))

