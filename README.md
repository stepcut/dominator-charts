Beautiful, interactive charts rendered in the web-browser canvas.

`Demo.hs` has a self-contained example. But, in order to be self contained, it also includes a bunch of irrelavant noise. Here are the important bits.

In your HTML you need to create a canvas element. Something a bit like this:

```
           <canvas id="scatter-plot" width="960" height="480"></canvas>

```

To draw something on that canvas, you use the `drawCanvas` function:

```
drawCanvas :: JSElement -> Canvas2D -> IO ()
```

The `JSElement` is the `<canvas>` node you want to draw on, and `Canvas2D` is the drawing instructions.

The chart library currently only supports line charts. Others will be
added on demand. Using the `linePlot` function you can generate a
`Canvas2D` value to pass to `drawCanvas`.

```
linePlot :: Double               -- ^ width in pixels
         -> Double               -- ^ height in pixels
         -> Scale                -- ^ x-scale
         -> [(Double, Canvas2D)] -- ^ x-axis labels, ascending order
         -> Scale                -- ^ y-scale
         -> [(Double, Canvas2D)] -- ^ y-axis labels, ascending order
         -> [LineSeries (Double, Double)]         -- ^ series
         -> Canvas2D
```

The `width` and `height` values should match the `width` and `height` values you declared in the `<canvas>` element.

The `x-scale` and `y-scale` values specify if you want `Linear` or `Log` scale on that axis.

the `x-axis labels` and `y-axis labels` are just what they sound like. The `Double` is the value you want to label. The `Canvas2D` is the label you want to use.

The labels can be any `Canvas2D` drawing. I need to create some helper functions. In the meantime, here is how you provide a text label,

```
   WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]
```

That label is intended to be used for the y-axis, and hence the `AlignRight`.

Here is an example for the `x-axis` which rotates the text by 90° so that you can use longer labels,

```
   WithContext2D [Font "18px Times", TextAlign AlignStart, Translate 0 20, Rotate (pi/2)] [ Draw (FillText (JSString.pack (showGregorian d)) 0 0 Nothing )]
```

Once again, I need to create some helper functions.

`[LineSeries (Double, Double)]` is the actually data series you want to plot. You can have more than one line series per chart, hence the `[]`.

`LineSeries` is defined as,

```
data LineSeries p = LineSeries
  { points    :: [p]
  , marker    :: [Canvas2D]
  , lineStyle :: ([Style], Double)
  }
```

`points` are the data-points you want to plot.

`marker` is the marker style you want to use for each point. At present, there is one marker style helper function,

```
circleMarker :: Color -- ^ marker color
             -> Double -- ^ radius
             -> [Canvas2D]
```

the `lineStyle` has two attributes. the `[Style]` allows you to specific color, gradient, pattern, etc. Though only color is currently implement. Others can be added upon request. The `Double` attribute is the line width.

## `linePlotDay`

The `linePlot` function expects data points in the form of `(Double, Double)`. However, we often want a date for the `x-axis`. The `linePlotDay` function makes that easy.

```
linePlotDay :: Double -- ^ width in pixels
            -> Double -- ^ height in pixels
            -> Scale  -- ^ x-scale
            -> Scale  -- ^ y-scale
            -> [(Double, Canvas2D)]       -- ^ y-axis labels, ascending order
            -> [LineSeries (Day, Double)] -- ^ points
            -> Canvas2D
```

Instead of `[LineSeries (Double, Double)]` it takes `[LineSeries (Day, Double)]`. It automatically generates the `x-axis` labels, so you only need to provide the `y-axis` labels.

This snippet shows this all put together:

```
     -- draw on the canvas
     (Just sp) <- getElementById d "scatter-plot"
     let december = [ fromGregorian 2015 12 d | d <- [1..31]  ]
         points = [ (day, (y*10)) | day <- december | y <- [1..31]]
         points2 = [ (day, (y*8)) | day <- december | y <- [1..31]]
         chart w h = linePlotDay w h
                     Linear
                     Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [1, 50, 100, 500]]
                       [ LineSeries points (circleMarker red 4.0) ([StyleColor red], 3.0)
                       , LineSeries points2 (circleMarker blue 4.0) ([StyleColor blue], 2.0)
                       ]

     drawCanvas sp (chart 960 480)
```

In this case, I use `getElementById` to locate the `<canvas>` element -- but any method is acceptable.

I think create some fake data values -- `december`, `points`, `points2`. I think call `linePlotDay` to create `chart`. And then `drawCanvas` to render that `chart`.

## Building the demo

The Demo.hs is not currently listed in the .cabal file. To build and run it:

```
 $ nix-shell
 [nix-shell] $ ghcjs Demo.hs
 [nix-shell] $ realpath Demo.jsexe/index.html
 /the/path/to/the/index.html
```

Copy the path to the `index.html` into your browser.
