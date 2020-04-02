{-# LANGUAGE OverloadedStrings #-}
module Graphics.Dominator.Charts.Line where

import Control.Monad.Trans (liftIO)
import Data.List (nub)
import Data.Text (Text, pack, unpack)
import qualified Data.JSString as JSString
import Graphics.Dominator.Charts.Core (Scale(Linear, Log), withChartPos)
import GHCJS.Types (JSRef(..), JSString(..))
import Data.Time.Calendar (Day, toGregorian, fromGregorian, diffDays, showGregorian)
import Chili.Canvas (Canvas2D(WithContext2D, Draw), Context2D(Font, LineWidth, Rotate, StrokeStyle, TextAlign, Translate), Draw(FillText, Stroke), Path2D(MoveTo, LineTo, Arc), Style, TextAlign(AlignStart))

{-

Log plot:

 - log of 0 is undefined.

 - log of 1 is 0, regardless of base

If we want the bottom left corner to be '0' then that means...

... if we divide by the min where min > 0...

-}

data LineSeries p = LineSeries
  { points    :: [p]
  , marker    :: [Canvas2D]
  , lineStyle :: ([Style], Double)
  }

linePlotDay :: Double -- ^ width in pixels
            -> Double -- ^ height in pixels
            -> Scale  -- ^ x-scale
            -> Scale  -- ^ y-scale
            -> [(Double, Canvas2D)]       -- ^ y-axis labels, ascending order
            -> [LineSeries (Day, Double)] -- ^ points
            -> Canvas2D
linePlotDay width height xScale yScale yLabels series' =
  let days         = map fst (concatMap points series')
      minDay       = minimum days -- FIXME: fails if list is empty
      dayToFloat d = case xScale of
                           Linear -> fromIntegral (diffDays d minDay)
                           (Log _) -> fromIntegral (diffDays d minDay) + 1
      dayLabel d   = WithContext2D [Font "18px Times", TextAlign AlignStart, Translate 0 20, Rotate (pi/2)] [ Draw (FillText (JSString.pack (showGregorian d)) 0 0 Nothing )]
      xLabels      = [ (dayToFloat d, dayLabel d) | (d ,_) <- (nub (concatMap points series')) ]
  in
    linePlot width height xScale xLabels yScale yLabels (map (convertSeries dayToFloat) series')
  where
    convertSeries :: (Day -> Double) -> LineSeries (Day, Double) -> LineSeries (Double, Double)
    convertSeries dayToFloat series =
      let days         = map fst (points series)
--          numDays      = diffDays (maximum days) minDay -- FIXME: fails if list is empty

          points'      = [ (dayToFloat d, v) | (d ,v) <- (points series) ]
      in series { points = points' }


linePlot :: Double               -- ^ width in pixels
         -> Double               -- ^ height in pixels
         -> Scale                -- ^ x-scale
         -> [(Double, Canvas2D)] -- ^ x-axis labels, ascending order
         -> Scale                -- ^ y-scale
         -> [(Double, Canvas2D)] -- ^ y-axis labels, ascending order
         -> [LineSeries (Double, Double)]         -- ^ series
         -> Canvas2D
linePlot width' height' xScale xLabels yScale yLabels series =
  let -- padding
      paddingTop = 20
      paddingBottom = 100
      paddingLeft = 110
      paddingRight = 20

      -- calculate height/width of chart after accounting for space for labels and whatnot
      height = height' - (paddingTop + paddingBottom)
      width  = width'  - (paddingLeft + paddingRight)
      xMin = minimum (map fst xLabels)
      xMax = maximum (map fst xLabels)
      yMin = minimum (map fst yLabels)
      yMax = maximum (map fst yLabels)
      yOffset = height - yMin

      -- map data point 'x' value to its pixel position in the chart
      toXPos xVal =
        case xScale of
         Linear -> paddingLeft + (xVal * (width / xDelta))
           where xDelta = xMax - xMin
         (Log base) -> paddingLeft + ((logBase base xVal) * (width / xDelta))
           where xDelta = logBase base xMax

      -- map data point 'y' value to its pixel position in the chart
      toYPos yVal =
        case yScale of
         Linear -> (height + paddingTop) - ((yVal - yMin) * (height / yDelta))
           where yDelta = yMax - yMin
         (Log base) -> (height + paddingTop) - ((logBase base yVal) * (height / yDelta))
           where yDelta = logBase base yMax

      -- draw the x-axis and labels
      drawXAxis height toXPos (x, label) =
        [ WithContext2D [Translate (toXPos x) (height + 10)] [ label ]
        , Draw (Stroke [ MoveTo (toXPos x) (height+paddingTop)
                       , LineTo (toXPos x) paddingTop
                       ])
        ]

      -- draw the y-axis and labels
      drawYAxis width toYPos (y, label) =
        [ WithContext2D [Translate (paddingLeft - 10) (toYPos y)] [ label ]
        , Draw (Stroke [ MoveTo (paddingLeft) (toYPos y)
                       , LineTo (paddingLeft + width) (toYPos y)
                       ])
        ]

      -- now draw the actual data points
  in WithContext2D []
       ([
        WithContext2D []
          (concat $ concat [ map (drawYAxis width toYPos) yLabels
                           , map (drawXAxis height toXPos) xLabels
                           ])
        ] ++ concatMap (drawSeries toXPos toYPos) series)

  where
    drawSeries :: (Double -> Double) -> (Double -> Double) -> LineSeries (Double, Double) -> [Canvas2D]
    drawSeries toXPos toYPos s =
       WithContext2D (LineWidth (snd $ lineStyle s) : (map StrokeStyle (fst $ lineStyle s))) (connectTheDots toXPos toYPos (points s)) :
        map (\p -> withChartPos toXPos toYPos p (marker s)) (points s)

connectTheDots _ _ [] = []
connectTheDots toXPos toYPos ((x,y):ps) =
  [ Draw (Stroke (MoveTo (toXPos x) (toYPos y) : [ LineTo (toXPos x') (toYPos y') | (x',y') <- ps ]))
  ]
