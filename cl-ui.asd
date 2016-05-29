(asdf:defsystem #:cl-ui
  :description "Common Lisp wrapper of libui"
  :author "Jinwoo Lee <jinwoo68@gmail.com>"
  :license ""
  :serial t
  :components ((:file "package")
               (:file "cl-ui-raw")
               (:file "cl-ui"))
  :depends-on (#:cffi
               #:trivial-main-thread))
