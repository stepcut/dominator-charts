{-# LANGUAGE OverloadedStrings #-}
module Graphics.Dominator.Charts.Core where

import Control.Monad.Trans (liftIO)
import Data.Text (Text, pack, unpack)
import qualified Data.JSString as JSString
import GHCJS.Types (JSRef(..), JSString(..))
import Data.Time.Calendar (Day, toGregorian, fromGregorian, diffDays, showGregorian)
import Chili.Canvas (Canvas2D(Draw, WithContext2D), Color(ColorName), Context2D(FillStyle, Translate), Draw(Fill, Stroke), Path2D(Arc), Style(StyleColor))

data Scale
  = Linear
  | Log Double -- ^ base
    deriving (Eq, Show, Read)


{-
plus x y = Draw (Stroke [ MoveTo   (x - 10) y
                              , LineTo   (x + 10) y
                              , MoveTo x (y - 10)
                              , LineTo x (y + 10)
                              ])

-}

-- | draw a circular marker with the specified color and diameter
circleMarker :: Color -- ^ marker color
             -> Double -- ^ radius
             -> [Canvas2D]
circleMarker color radius =
  let arc = Arc 0 0 radius 0 (2*pi) False
  in
  [ WithContext2D [FillStyle (StyleColor color)]
     [ Draw (Stroke [ arc ]) -- border
     , Draw (Fill [ arc ])   -- fill
     ]
  ]

withChartPos :: (Double -> Double) -> (Double -> Double) -> (Double, Double) -> ([Canvas2D] -> Canvas2D)
withChartPos toXPos toYPos (x, y)  =
  WithContext2D [Translate (toXPos x) (toYPos y)]
