(defsystem #:trivial-gamekit-tiled
  :version "0.0.0"
  :description "Tiled/trivial-gamekit integration."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "trivial-gamekit-tiled"))
  :depends-on
  (#:alexandria
   #:trivial-gamekit
   #:cl-tiled))


(defsystem #:trivial-gamekit-tiled/example
  :version "0.0.0"
  :description "Example of tiled/trivial-gamekit integration."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "example"))
  :depends-on
  (#:alexandria
   #:trivial-gamekit-tiled
   #:cl-tiled))
