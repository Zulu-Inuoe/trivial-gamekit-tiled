(defpackage #:trivial-gamekit-tiled.example
  (:use #:cl #:gamekit)
  (:import-from #:alexandria
                #:assoc-value
                #:when-let
                #:if-let)
  (:export #:run))

(in-package #:trivial-gamekit-tiled.example)


(trivial-gamekit-tiled:define-tilemap orthogonal-outside
    (asdf:system-relative-pathname :trivial-gamekit-tiled/example
                                   "assets/orthogonal-outside.tmx")
  :encoding :latin-1)


(defgame tiled-example ()
  ((tilemap :initform nil)))


(defmethod post-initialize :after ((this tiled-example))
  (with-slots (tilemap) this
    (setf tilemap (trivial-gamekit-tiled:load-tilemap 'orthogonal-outside))))


(defun run ()
  (gamekit:start 'tiled-example)
  (gamekit:gamekit))


(defmethod draw ((app tiled-example))
  (with-slots (tilemap) app
    (trivial-gamekit-tiled:draw-tilemap tilemap)))
