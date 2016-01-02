{-# LANGUAGE OverloadedStrings #-}
module Graphics.Isomaniac.Charts.Core where

import Control.Monad.Trans (liftIO)
import Data.Text (Text, pack, unpack)
import qualified Data.JSString as JSString
import GHCJS.Types (JSRef(..), JSString(..))
import Data.Time.Calendar (Day, toGregorian, fromGregorian, diffDays, showGregorian)
import Web.ISO.Types

data Scale
  = Linear
  | Log
    deriving (Eq, Show, Read)
scatterPlotDay :: Double -- ^ width in pixels
               -> Double -- ^ height in pixels
               -> Scale  -- ^ x-scale
               -> Scale  -- ^ y-scale
               -> [(Double, Canvas2D)] -- ^ y-axis labels, ascending order
               -> [(Day, Double)] -- ^ points
               -> Canvas2D
scatterPlotDay width height xScale yScale yLabels points =
  let days         = map fst points
      minDay       = minimum days -- FIXME: fails if list is empty
      numDays      = diffDays (maximum days) minDay -- FIXME: fails if list is empty
      dayToFloat d = fromIntegral (diffDays d minDay)
      dayLabel d   = WithContext2D [Font "18px Times", TextAlign AlignStart, Translate 0 20, Rotate (pi/2)] [ Draw (FillText (JSString.pack (showGregorian d)) 0 0 Nothing)]
      xLabels      = [ (dayToFloat d, dayLabel d) | (d ,_) <- points ]
      points'      = [ (dayToFloat d, v) | (d ,v) <- points ]
  in
   scatterPlot width height xScale xLabels yScale yLabels points'

scatterPlot :: Double -- ^ width in pixels
            -> Double -- ^ height in pixels
            -> Scale -- ^ x-scale
            -> [(Double, Canvas2D)] -- ^ x-axis labels, ascending order
            -> Scale -- ^ y-scale
            -> [(Double, Canvas2D)] -- ^ y-axis labels, ascending order
            -> [(Double, Double)] -- ^ points
            -> Canvas2D
scatterPlot width' height' xScale xLabels yScale yLabels points =
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
      xDelta = xMax - xMin
      yDelta = yMax - yMin
      yOffset = height - yMin
      toYPos yVal = (height + paddingTop) - ((yVal - yMin) * (height / yDelta))
      toXPos xVal = paddingLeft + (xVal * (width / xDelta))
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
       [ -- Draw (FillText "A Scatter Plot" 0.5 20.5 Nothing)
--       , Draw (FillText "A Scatter Plot" 1 40 Nothing)
        WithContext2D []
          (concat $ concat [ map (drawYAxis width toYPos) yLabels
                           , map (drawXAxis height toXPos) xLabels
                           ])
       , WithContext2D [FillStyle (StyleColor (ColorName "green"))]
         (concatMap (drawPoint toXPos toYPos) points)
       ]

  where
    drawPoint toXPos toYPos (x, y) =
      let arc = Arc (toXPos x) (toYPos y) 3.0 0 (2*pi) False
      in
      [ Draw (Stroke [ arc ])
      , Draw (Fill [ arc ])
      ]
