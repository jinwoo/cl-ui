(asdf:defsystem #:control-gallery
  :description "Demo app using cl-ui"
  :author "Jinwoo Lee <jinwoo68@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "control-gallery"))
  :depends-on (#:cl-ui))
