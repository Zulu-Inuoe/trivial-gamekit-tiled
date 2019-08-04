(defpackage #:trivial-gamekit-tiled
  (:use
   #:cl
   #:trivial-gamekit)
  (:import-from
   #:alexandria
   #:assoc-value
   #:when-let
   #:if-let)
  (:export
   #:define-tilemap
   #:load-tilemap
   #:draw-tilemap))

(in-package #:trivial-gamekit-tiled)

(defvar *tilemap-resource-map* (make-hash-table :test #'equal))
(defvar *zero-vec* (vec2 0 0))

(defun register-tilemap-resource (name path)
  (let ((path (namestring path)))
    (setf (gethash path *tilemap-resource-map*) name
          (gethash name *tilemap-resource-map*) path)))

(defun find-resource-name (path)
  (let ((path (namestring path)))
    (gethash path *tilemap-resource-map*)))

(defun find-resource-path (name)
  (gethash name *tilemap-resource-map*))

(defun tiled-color->color (c)
  "Convert from a `cl-tiled:tiled-color' into a `gamekit:vec4' rgba [0, 1] color"
  (flet ((8bpp->float (c)
           (float (/ c 256))))
    (vec4 (8bpp->float (cl-tiled:tiled-color-r c))
          (8bpp->float (cl-tiled:tiled-color-g c))
          (8bpp->float (cl-tiled:tiled-color-b c))
          (8bpp->float (cl-tiled:tiled-color-a c)))))

(defun cons->vec2 (cons)
  (vec2 (car cons) (cdr cons)))

(defun tileset-name (tilemap-name tileset)
  (alexandria:symbolicate tilemap-name  '$tileset$ (cl-tiled:tileset-name tileset)))

(defun image-name (tilemap-name tileset)
  (alexandria:symbolicate tilemap-name  '$image$ (cl-tiled:tileset-name tileset)))

(defmacro define-tilemap (name path &key encoding)
  (let* ((path (eval path))
         (map (cl-tiled:load-map path)))
    (alexandria:once-only (encoding)
      `(progn
         (define-text ,name ,path :encoding ,encoding)
         (register-tilemap-resource ',name ,path)
         ,@(loop for tileset in (cl-tiled:map-tilesets map)
                 for image = (cl-tiled:tileset-image tileset)
                 when (subtypep (class-of tileset) 'cl-tiled:external-tileset)
                   append (let ((tileset-name (tileset-name name tileset))
                                (tileset-path (cl-tiled:tileset-source tileset)))
                            `((define-text ,tileset-name ,tileset-path :encoding ,encoding)
                              (register-tilemap-resource ',tileset-name ,tileset-path)))
                 when image
                   append (let ((image-name (image-name name tileset))
                                (image-path (cl-tiled:image-source image)))

                            `((define-image ,image-name ,image-path
                                :use-nearest-interpolation t)
                              (register-tilemap-resource ',image-name ,image-path))))))))

(defun load-tilemap (name)
  (when-let ((resource-path (find-resource-path name)))
    (flet ((%load-resource (path)
             (get-text (find-resource-name path))))
      (cl-tiled:load-map resource-path #'%load-resource))))

(defmacro without-antialiased-shapes (&body body)
  `(unwind-protect
        (progn
          (ge.vg:antialias-shapes nil)
          ,@body)
     (ge.vg:antialias-shapes t)))

(defun draw-tilemap (tilemap)
  (draw tilemap))

(defmethod draw :around ((layer cl-tiled:layer))
  ;; Don't draw invisible layers
  (when (cl-tiled:layer-visible layer)
    (with-pushed-canvas ()
      ;; Offset layer x/y
      (translate-canvas (cl-tiled:layer-offset-x layer)
                        (cl-tiled:layer-offset-y layer))
      (call-next-method))))

(defmethod draw ((cell cl-tiled:cell))
  (let* ((tile (cl-tiled:cell-tile cell))
         (tile-image (cl-tiled:tile-image tile)))
    (when tile-image
      (let ((image (find-resource-name (cl-tiled:image-source tile-image)))
            (flip-x (cl-tiled:cell-flipped-horizontal cell))
            (flip-y (cl-tiled:cell-flipped-vertical cell))
            (tile-width (cl-tiled:tile-width tile))
            (tile-height (cl-tiled:tile-height tile)))
        (with-pushed-canvas ()
          (translate-canvas
           (+ (cl-tiled:cell-x cell) (if flip-x tile-width 0))
           (+ (cl-tiled:cell-y cell) (if flip-y 0 tile-height)))
          (scale-canvas (if flip-x -1 1) (if flip-y 1 -1))
          (without-antialiased-shapes
            (draw-image *zero-vec*
                        image
                        :origin (vec2 (cl-tiled:tile-pixel-x tile)
                                      (- (image-height image) tile-height (cl-tiled:tile-pixel-y tile)))
                        :width tile-width
                        :height tile-height)))))))

(defmethod draw ((layer cl-tiled:tile-layer))
  (dolist (cell (cl-tiled:layer-cells layer))
    (draw cell)))

(defmethod draw ((layer cl-tiled:image-layer))
  ;; TODO Draw image layers
  )

(defvar *object-color* (vec4 0 0 0 1)
  "Color to draw objects with.")

(defmethod draw :around ((object cl-tiled:object))
  (with-pushed-canvas ()
    (translate-canvas (cl-tiled:object-x object)
                      (cl-tiled:object-y object))
    (call-next-method)))

(defmethod draw ((object cl-tiled:ellipse-object))
  ;; TODO Need to fix ellipse-rx and ry so they're halved coming out of cl-tiled
  (let ((rx (cl-tiled:ellipse-rx object))
        (ry (cl-tiled:ellipse-ry object)))
    (translate-canvas rx ry)
    (draw-ellipse *zero-vec* rx ry
                  :stroke-paint *object-color*)))

(defmethod draw ((object cl-tiled:rect-object))
  (draw-rect *zero-vec*
             (cl-tiled:rect-width object)
             (cl-tiled:rect-height object)
             :stroke-paint *object-color*))

(defmethod draw ((object cl-tiled:polygon-object))
  (draw-polygon (mapcar #'cons->vec2 (cl-tiled:polygon-vertices object))
                :stroke-paint *object-color*))

(defmethod draw ((object cl-tiled:polyline-object))
  (draw-polyline (mapcar #'cons->vec2 (cl-tiled:polyline-points object))
                 *object-color*))

(defmethod draw ((object cl-tiled:tile-object))
  ;; TODO
  )

(defmethod draw ((object cl-tiled:text-object))
  ;; TODO
  )

(defmethod draw ((object cl-tiled:image-object))
  )

(defmethod draw ((layer cl-tiled:object-layer))
  ;;TODO Might want to care about draw order, but maybe `cl-tiled' should order them for us.
  (dolist (object (cl-tiled:object-group-objects layer))
    (draw object)))

(defmethod draw ((layer cl-tiled:group-layer))
  (dolist (layer (cl-tiled:group-layers layer))
    (draw layer)))

(defmethod draw ((map cl-tiled:tiled-map))
  "Draw a `cl-tiled:tiled-map' by rendering its background color and overlaying layers."
  (draw-rect (vec2 0 0) (cl-tiled:map-width-pixels map) (cl-tiled:map-height-pixels map)
             :fill-paint (tiled-color->color (cl-tiled:map-background-color map)))

  (with-pushed-canvas ()
    (translate-canvas 0 (canvas-height))
    (scale-canvas 1 -1)
    ;; Draw out each layer
    (dolist (layer (cl-tiled:map-layers map))
      (draw layer))))
