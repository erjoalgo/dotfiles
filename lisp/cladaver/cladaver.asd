(asdf:defsystem
 :cladaver
 :serial t
 :description "A common lisp minimal webdav client."
 :license "GPLv3"
 :author "Ernesto Alfonso <erjoalgo@gmail.com>"
 :depends-on
 (
  #:xpath
  #:drakma
  #:cxml
  #:cxml-stp
  #:statusor)
 :components
 ((:file "cladaver")))
