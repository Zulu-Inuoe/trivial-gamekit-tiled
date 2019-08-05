(defpackage #:trivial-gamekit-tiled
  (:use
   #:cl
   #:trivial-gamekit)
  (:import-from
   #:alexandria
   #:assoc-value
   #:when-let
   #:when-let*
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

(defun degrees->radians (degrees)
  (/ (* pi degrees) 180))

(defun tileset-name (tilemap-name tileset)
  (alexandria:symbolicate tilemap-name  '$tileset$ (cl-tiled:tileset-name tileset)))

(defun tileset-image-name (tilemap-name tileset)
  (alexandria:symbolicate tilemap-name  '$tileset-image$ (cl-tiled:tileset-name tileset)))

(defun tileset-tile-image-name (tilemap-name tileset tile)
  (alexandria:symbolicate tilemap-name '$tileset-tile-image$ (cl-tiled:tileset-name tileset) "[" (princ-to-string (cl-tiled:tile-id tile)) "]"))

(defun image-layer-image-name (tilemap-name layer)
  (alexandria:symbolicate tilemap-name '$layer-image$ (cl-tiled:layer-name layer)))

(defmacro define-tilemap (name path &key encoding)
  (let* ((path (eval path))
         (map (cl-tiled:load-map path)))
    (alexandria:once-only (encoding)
      `(progn
         (define-text ,name ,path :encoding ,encoding)
         (register-tilemap-resource ',name ,path)
         ;; Register each tileset with an image
         ,@(loop for tileset in (cl-tiled:map-tilesets map)
                 for image = (cl-tiled:tileset-image tileset)
                 when (typep tileset 'cl-tiled:external-tileset)
                   append (let ((tileset-name (tileset-name name tileset))
                                (tileset-path (cl-tiled:tileset-source tileset)))
                            `((define-text ,tileset-name ,tileset-path :encoding ,encoding)
                              (register-tilemap-resource ',tileset-name ,tileset-path)))
                 when image
                   append (let ((image-name (tileset-image-name name tileset))
                                (image-path (cl-tiled:image-source image)))

                            `((define-image ,image-name ,image-path
                                :use-nearest-interpolation t)
                              (register-tilemap-resource ',image-name ,image-path)))
                 append (loop for tile in (cl-tiled:tileset-tiles tileset)
                              for image = (cl-tiled:tile-image tile)
                              when (and (typep tile 'cl-tiled:tiled-tileset-image-tile)
                                        (typep image 'cl-tiled:external-tiled-image))
                                append (let ((image-name (tileset-tile-image-name name tileset tile))
                                             (image-path (cl-tiled:image-source image)))
                                         `((define-image ,image-name ,image-path
                                             :use-nearest-interpolation t)
                                           (register-tilemap-resource ',image-name ,image-path)))))
         ;; Hunt down any image layers and register their images as well
         ,@(let ((forms nil))
             (labels ((recurse (layer)
                        (typecase layer
                          (cl-tiled:image-layer
                           (when-let* ((image (cl-tiled:layer-image layer))
                                       (image-path (and (typep image 'cl-tiled:external-tiled-image)
                                                        (cl-tiled:image-source image)))
                                       (image-name (image-layer-image-name name layer)))
                             (push `(define-image ,image-name ,image-path
                                      :use-nearest-interpolation t)
                                   forms)
                             (push `(register-tilemap-resource ',image-name ,image-path)
                                   forms)))
                          (cl-tiled:group-layer
                           (map nil #'recurse (cl-tiled:group-layers layer))))))
               (map nil #'recurse (cl-tiled:map-layers map)))
             forms)))))

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

(defun draw-tile (tile origin &key flip-x flip-y width height)
  (when-let ((image (find-resource-name (cl-tiled:image-source (cl-tiled:tile-image tile)))))
    (multiple-value-bind (tile-origin tile-width tile-height)
        (etypecase tile
          (cl-tiled:tiled-tileset-image-tile
           (values
            *zero-vec*
            (image-width image)
            (image-height image)))
          (t
           (values
            (vec2 (cl-tiled:tile-pixel-x tile)
                  (- (image-height image) (cl-tiled:tile-height tile) (cl-tiled:tile-pixel-y tile)))
            (cl-tiled:tile-width tile)
            (cl-tiled:tile-height tile))))
      (with-pushed-canvas ()
        (when width
          (scale-canvas (/ width tile-width) 1))
        (when height
          (scale-canvas 1 (/ height tile-height)))
        (translate-canvas
         (+ (x origin) (if flip-x tile-width 0))
         (+ (y origin) (if flip-y (- tile-height) 0)))
        (scale-canvas (if flip-x -1 1) (if flip-y 1 -1))
        (without-antialiased-shapes
          (draw-image *zero-vec* image
                      :origin tile-origin
                      :width tile-width
                      :height tile-height))))))

(defun draw-tilemap (tilemap)
  (draw tilemap))

(defmethod draw :around ((layer cl-tiled:layer))
  ;; Don't draw invisible layers
  (when (cl-tiled:layer-visible layer)
    (with-pushed-canvas ()
      ;; Offset layer x/y
      (translate-canvas (cl-tiled:layer-offset-x layer)
                        (cl-tiled:layer-offset-y layer))
      (ge.vg:with-alpha ((cl-tiled:layer-opacity layer))
        (call-next-method)))))

(defmethod draw ((cell cl-tiled:cell))
  (with-pushed-canvas ()
    (translate-canvas (cl-tiled:cell-x cell) (cl-tiled:cell-y cell))
    (draw-tile (cl-tiled:cell-tile cell) *zero-vec*
               :flip-x (cl-tiled:cell-flipped-horizontal cell)
               :flip-y (cl-tiled:cell-flipped-vertical cell))))

(defmethod draw ((layer cl-tiled:tile-layer))
  (with-pushed-canvas ()
    (translate-canvas 0 (cl-tiled:layer-tile-height layer))
    (dolist (cell (cl-tiled:layer-cells layer))
      (draw cell))))

(defmethod draw ((layer cl-tiled:image-layer))
  (when-let ((image (find-resource-name (cl-tiled:image-source (cl-tiled:layer-image layer)))))
    (with-pushed-canvas ()
      (translate-canvas 0 (image-height image))
      (scale-canvas 1 -1)
      (without-antialiased-shapes
        (draw-image *zero-vec* image)))))

(defvar *object-color* (vec4 0 0 0 1)
  "Color to draw objects with.")

(defmethod draw :around ((object cl-tiled:object))
  (when (cl-tiled:object-visible object)
    (with-pushed-canvas ()
      (translate-canvas (cl-tiled:object-x object)
                        (cl-tiled:object-y object))
      (rotate-canvas (degrees->radians (cl-tiled:object-rotation object)))
      (call-next-method))))

(defmethod draw ((object cl-tiled:ellipse-object))
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
  (let ((tile (cl-tiled:object-tile object)))
    (draw-tile tile *zero-vec*
               :flip-x (cl-tiled:object-flipped-horizontal object)
               :flip-y (cl-tiled:object-flipped-vertical object)
               :width (cl-tiled:object-width object)
               :height (cl-tiled:object-height object))))

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
