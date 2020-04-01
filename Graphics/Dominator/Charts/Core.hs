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
  | Log Double -- ^ base
    deriving (Eq, Show, Read)


{-
plus x y = Draw (Stroke [ MoveTo   (x - 10) y
                              , LineTo   (x + 10) y
                              , MoveTo x (y - 10)
                              , LineTo x (y + 10)
                              ])

-}
circleMarker :: JSString -> Double -> [Canvas2D]
circleMarker color diameter =
  let arc = Arc 0 0 diameter 0 (2*pi) False
  in
  [ WithContext2D [FillStyle (StyleColor (ColorName color))]
     [ Draw (Stroke [ arc ])
     , Draw (Fill [ arc ])
     ]
  ]

drawMarker :: (Double -> Double) -> (Double -> Double) -> [Canvas2D] -> (Double, Double) -> Canvas2D
drawMarker toXPos toYPos marker (x, y)  =
  WithContext2D [Translate (toXPos x) (toYPos y)]
    marker
