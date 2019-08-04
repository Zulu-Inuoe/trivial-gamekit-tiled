(defsystem #:trivial-tiled
  :version "0.0.0"
  :description "Tiled/trivial-gamekit integration."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "trivial-tiled"))
  :depends-on
  (#:alexandria
   #:trivial-gamekit
   #:cl-tiled))
