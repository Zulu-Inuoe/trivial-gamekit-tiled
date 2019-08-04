(defpackage #:trivial-tiled
  (:use
   #:cl
   #:trivial-gamekit)
  (:import-from
   #:alexandria
   #:assoc-value
   #:when-let
   #:if-let)
  (:export
   #:current-map-path
   #:run))

(in-package #:trivial-tiled)

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

(defgame tiled-example ()
  ((current-map-path
    :initform nil
    :accessor current-map-path)
   (current-map
    :initform nil
    :accessor current-map)
   (resource-mappings
    :initform nil
    :accessor resource-mappings
    :documentation "An alist of (thing . resource-id). Used to map a `cl-tiled:tiled-image' to a resource-id (a symbol)")))

(defun run (&optional map-path)
  (gamekit:start 'tiled-example)
  (when map-path
    (setf (current-map-path (gamekit)) map-path))
  (gamekit))

(defun put-resource (thing resource)
  (push (cons thing resource) (resource-mappings (gamekit))))

(defun get-resource (thing)
  (assoc-value (resource-mappings (gamekit)) thing))

(defmethod (setf current-map-path) :after (value (app tiled-example))
  "Try loading the map after updating the path."
  (let (success)
    (unwind-protect (setf (current-map app) (cl-tiled:load-map value)
                          success t)
      (unless success
        (setf (current-map app) nil)))))

(defmethod (setf current-map) :before (value (app tiled-example))
  ;; Unregister any previous resource
  (setf (resource-mappings app) nil)
  (when value
    (dolist (tileset (cl-tiled:map-tilesets value))
      ;; TODO: Need to support images in individual tiles as opposed to just in tilesets.
      (let ((image (cl-tiled:tileset-image tileset)))
        (when image
          ;; NOTE: Hacky way to get runtime image loading by dynamically defining new resources
          (let ((sym (intern (cl-tiled:tileset-name tileset) :trivial-tiled)))
            (gamekit::register-game-resource sym (cl-tiled:image-source image)
                                             '(:use-nearest-interpolation t)
                                             :image :type :png)
            (gamekit::autoprepare sym)
            (put-resource image sym)))))))

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
         (image (get-resource (cl-tiled:tile-image tile))))
    (when image
      (let ((flip-x (cl-tiled:cell-flipped-horizontal cell))
            (flip-y (cl-tiled:cell-flipped-vertical cell))
            (tile-width (cl-tiled:tile-width tile))
            (tile-height (cl-tiled:tile-height tile)))
        (with-pushed-canvas ()
          (translate-canvas
           (+ (cl-tiled:cell-x cell) (if flip-x tile-width 0))
           (+ (cl-tiled:cell-y cell) (if flip-y 0 tile-height)))
          (scale-canvas (if flip-x -1 1) (if flip-y 1 -1))
          (draw-image (vec2 0 0)
                      image
                      :origin (vec2 (cl-tiled:tile-pixel-x tile)
                                    (- (image-height image) tile-height (cl-tiled:tile-pixel-y tile)))
                      :width tile-width
                      :height tile-height))))))

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
  (let ((rx (/ (cl-tiled:ellipse-rx object) 2))
        (ry (/ (cl-tiled:ellipse-ry object) 2)))
    (draw-ellipse (vec2 rx ry)
                  rx ry
                  :stroke-paint *object-color*)))

(defmethod draw ((object cl-tiled:rect-object))
  (draw-rect (vec2 0 0)
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

(defmethod draw ((app tiled-example))
  (when (current-map app)
    (handler-case (draw (current-map app))
      (error (e)
        (warn "Failed to draw ~A:~%~A~%" (current-map app) e)
        (setf (current-map app) nil))))
  (when (current-map-path app)
    (draw-text (namestring (current-map-path app)) (vec2 5 (- (canvas-height) 20)))))

