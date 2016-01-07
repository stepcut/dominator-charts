{-# LANGUAGE OverloadedStrings, ParallelListComp, QuasiQuotes #-}
module Main where

import qualified Data.JSString as JSString
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day, toGregorian, fromGregorian, diffDays, showGregorian)
import Graphics.Isomaniac.Charts.Core
import Graphics.Isomaniac.Charts.Scatter
import Language.Haskell.HSX.QQ (hsx)
import System.Random (randoms, mkStdGen)
import Web.ISO.HSX
import Web.ISO.Murv
import Web.ISO.Types

{-
december = [ fromGregorian 2015 12 d | d <- [1..31]  ]
      points = [ (day, y*100) | day <- december | y <- take 31 (randoms (mkStdGen (c + 1)))]
--      canvas w h = scatterPlot w h Linear [ (x, WithContext2D [Font "18px Times", TextAlign AlignStart, Translate 0 20, Rotate (pi/2)] [ Draw (FillText (JSString.pack (show x)) 0 0 Nothing)]) | x <- [0, 20, 40, 60, 70, 80, 100]]
--                                   Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [0, 20, 40, 60, 70, 80, 100]] points
      canvas w h = scatterPlotDay w h
                     Linear
                     Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [0, 20, 40, 60, 70, 80, 100]] points

-}
data Model = Model
data Action
  = Action
  | ServerResponse Text
  deriving Show

update' :: Action -> Model -> (Model, Maybe Text)
update' action model = (model, Nothing)

view' :: Model -> (HTML Action, [Canvas])
view' Model =
  let december = [ fromGregorian 2015 12 d | d <- [1..31]  ]
      points = [ (day, (y*10)) | day <- december | y <- [1..31]]
      canvas w h = scatterPlotDay w h
                     Linear
                     Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [1, 10, 11, 50, 100, 500]] (circleMarker "red" 4.0) points

  in
   ([hsx| <div>
           <h1>Scatter Plot</h1>
           <canvas id="canvas" width="960" height="480"></canvas>
         </div>
        |], [Canvas "canvas" (canvas 960 480)])

demo :: MURV Model Action Text
demo = MURV
  { model = Model
  , update = update'
  , view = view'
  }

main :: IO ()
main = murv "http://localhost:8000/api" ServerResponse demo Nothing
