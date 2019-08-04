# trivial-tiled

Playing around by rendering a [Tiled](http://www.mapeditor.org) map in [trivial-gamekit](https://github.com/borodust/trivial-gamekit).

## Running

Calling `(trivial-tiled:run)` will run the application (optionally specify a map to load).
Switch maps by `setf`ing `trivial-tiled:current-map-path` on the returned app.
