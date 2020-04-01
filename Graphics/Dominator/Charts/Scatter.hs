{-# LANGUAGE OverloadedStrings #-}
module Graphics.Isomaniac.Charts.Scatter where

import Control.Monad.Trans (liftIO)
import Data.Text (Text, pack, unpack)
import qualified Data.JSString as JSString
import Graphics.Isomaniac.Charts.Core (Scale(Linear, Log), drawMarker)
import GHCJS.Types (JSRef(..), JSString(..))
import Data.Time.Calendar (Day, toGregorian, fromGregorian, diffDays, showGregorian)
import Web.ISO.Types

{-

Log plot:

 - log of 0 is undefined.

 - log of 1 is 0, regardless of base

If we want the bottom left corner to be '0' then that means...

... if we divide by the min where min > 0...

-}

scatterPlotDay :: Double -- ^ width in pixels
               -> Double -- ^ height in pixels
               -> Scale  -- ^ x-scale
               -> Scale  -- ^ y-scale
               -> [(Double, Canvas2D)] -- ^ y-axis labels, ascending order
               -> [Canvas2D] -- ^ draw a marker
               -> [(Day, Double)] -- ^ points
               -> Canvas2D
scatterPlotDay width height xScale yScale yLabels marker points =
  let days         = map fst points
      minDay       = minimum days -- FIXME: fails if list is empty
      numDays      = diffDays (maximum days) minDay -- FIXME: fails if list is empty
      dayToFloat d = case xScale of
                          Linear -> fromIntegral (diffDays d minDay)
                          (Log _) -> fromIntegral (diffDays d minDay) + 1
      dayLabel d   = WithContext2D [Font "18px Times", TextAlign AlignStart, Translate 0 20, Rotate (pi/2)] [ Draw (FillText (JSString.pack (showGregorian d)) 0 0 Nothing)]
      xLabels      = [ (dayToFloat d, dayLabel d) | (d ,_) <- points ]
      points'      = [ (dayToFloat d, v) | (d ,v) <- points ]
  in
   scatterPlot width height xScale xLabels yScale yLabels marker points'

scatterPlot :: Double -- ^ width in pixels
            -> Double -- ^ height in pixels
            -> Scale -- ^ x-scale
            -> [(Double, Canvas2D)] -- ^ x-axis labels, ascending order
            -> Scale -- ^ y-scale
            -> [(Double, Canvas2D)] -- ^ y-axis labels, ascending order
            -> [Canvas2D] -- ^ draw a marker
            -> [(Double, Double)] -- ^ points
            -> Canvas2D
scatterPlot width' height' xScale xLabels yScale yLabels marker points =
  let paddingTop = 20
      paddingBottom = 100
      paddingLeft = 110
      paddingRight = 20
      height = height' - (paddingTop + paddingBottom)
      width  = width'  - (paddingLeft + paddingRight)
      xMin = minimum (map fst xLabels)
      xMax = maximum (map fst xLabels)
      yMin = minimum (map fst yLabels)
      yMax = maximum (map fst yLabels)
      yOffset = height - yMin
      toXPos xVal =
        case xScale of
         Linear -> paddingLeft + (xVal * (width / xDelta))
           where xDelta = xMax - xMin
         (Log base) -> paddingLeft + ((logBase base xVal) * (width / xDelta))
           where xDelta = logBase base xMax
      toYPos yVal =
        case yScale of
         Linear -> (height + paddingTop) - ((yVal - yMin) * (height / yDelta))
           where yDelta = yMax - yMin
         (Log base) -> (height + paddingTop) - ((logBase base yVal) * (height / yDelta))
           where yDelta = logBase base yMax
      drawXAxis height toXPos (x, label) =
        [ WithContext2D [Translate (toXPos x) height] [ label ]
        , Draw (Stroke [ MoveTo (toXPos x) (height+paddingTop)
                       , LineTo (toXPos x) paddingTop
                       ])
        ]
      drawYAxis width toYPos (y, label) =
        [ WithContext2D [Translate paddingLeft (toYPos y)] [ label ]
        , Draw (Stroke [ MoveTo (paddingLeft) (toYPos y)
                       , LineTo (paddingLeft + width) (toYPos y)
                       ])
        ]

  in WithContext2D []
       ([ -- Draw (FillText "A Scatter Plot" 0.5 20.5 Nothing)
--       , Draw (FillText "A Scatter Plot" 1 40 Nothing)
        WithContext2D []
          (concat $ concat [ map (drawYAxis width toYPos) yLabels
                           , map (drawXAxis height toXPos) xLabels
                           ])
       ] ++ (map (drawMarker toXPos toYPos marker) points))

