# Save your animation at a desired quality.

This function is a helepr for `gganimate`'s `anim_save()`, providing a
simplified, though less feature-rich, version of these functions.
Animations are saved as `.gif`s at the desired path. With this function,
publication-quality (high-resolution and smooth) animations are
possible, but take a long time to render.

## Usage

``` r
export_animation(
  anim_object,
  path,
  duration = 30,
  fps = 10,
  width = 7.5,
  height = 5.5,
  dpi = 100
)
```

## Arguments

- anim_object:

  A `gganimate` object.

- path:

  A string representing the desired path and name at which to save
  animation.

- duration:

  Optional. A numeric, in seconds, representing the length of the
  animation. Default is 30.

- fps:

  Optional. The frames per second of the saved animation. Default is 10.

- width:

  Optional. The width of the exported image, in inches. Default is 7.5

- height:

  Optional. The height of the exported image, in inches. Default is 5.5.

- dpi:

  Optional. The resolution, in dots per inch, of the image. Default is
  100.
