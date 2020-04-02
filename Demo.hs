{-# LANGUAGE OverloadedStrings, ParallelListComp, QuasiQuotes, TemplateHaskell #-}
module Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar)

import Chili.Canvas
import Chili.Canvas.Color (red, blue)
import Chili.Types
import qualified Data.JSString as JSString
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day, toGregorian, fromGregorian, diffDays, showGregorian)

import Dominator
import Dominator.Types (Attr(..), Html(..), JSDocument, DHandle)
import Dominator.HSX
import Dominator.DOMC
import Dominator.JSDOM

import Graphics.Dominator.Charts.Core
import Graphics.Dominator.Charts.Line
import Language.Haskell.HSX.QQ (hsx)
import System.Random (randoms, mkStdGen)
{-
import Web.ISO.HSX
import Web.ISO.Murv
import Web.ISO.Types
-}

{-
december = [ fromGregorian 2015 12 d | d <- [1..31]  ]
      points = [ (day, y*100) | day <- december | y <- take 31 (randoms (mkStdGen (c + 1)))]
--      canvas w h = scatterPlot w h Linear [ (x, WithContext2D [Font "18px Times", TextAlign AlignStart, Translate 0 20, Rotate (pi/2)] [ Draw (FillText (JSString.pack (show x)) 0 0 Nothing)]) | x <- [0, 20, 40, 60, 70, 80, 100]]
--                                   Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [0, 20, 40, 60, 70, 80, 100]] points
      canvas w h = scatterPlotDay w h
                     Linear
                     Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [0, 20, 40, 60, 70, 80, 100]] points

-}

{-
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
-}

data Model = Model deriving Show

initModel :: Model
initModel = Model

template :: JSDocument -> JSNode -> IO (Model -> IO ())
template =
  [domc|<html>
         <head>
          <title>Fancy Charts in the Canvas</title>
         </head>
         <body>
           <h1>Line Plot</h1>
           <canvas id="scatter-plot" width="960" height="480"></canvas>
         </body>
        </html>
      |]

{-
view :: DHandle -> TVar Model -> IO Html
view domH mModel =
  let december = [ fromGregorian 2015 12 d | d <- [1..31]  ]
      points = [ (day, (y*10)) | day <- december | y <- [1..31]]
      canvas w h = scatterPlotDay w h
                     Linear
                     Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [1, 10, 11, 50, 100, 500]] (circleMarker "red" 4.0) points

  in
    pure $ [hsx|
              <div>
                <h1>Scatter Plot</h1>
                <canvas id="canvas" width="960" height="480"></canvas>
              </div>
              |]
-}
main :: IO ()
main =
  do -- initialize domc
     (Just d) <- currentDocument
     model <- newTVarIO initModel
     (Just elems) <- getElementsByTagName d "html"
     (Just rootNode) <- item elems 0
     update <- template d rootNode
     update =<< (atomically $ readTVar model)

     -- draw on the canvas
     (Just sp) <- getElementById d "scatter-plot"
     let december = [ fromGregorian 2015 12 d | d <- [1..31]  ]
         points = [ (day, (y*10)) | day <- december | y <- [1..31]]
         points2 = [ (day, (y*8)) | day <- december | y <- [1..31]]
         canvas w h = linePlotDay w h
                     Linear
                     Linear [ (y, WithContext2D [Font "18px Times", TextAlign AlignRight] [Draw (FillText (JSString.pack (show y)) 0 0 Nothing)]) | y <- [1, 50, 100, 500]]
                       [ LineSeries points (circleMarker red 4.0) ([StyleColor red], 3.0)
                       , LineSeries points2 (circleMarker blue 4.0) ([StyleColor blue], 2.0)
                       ]

     drawCanvas sp (canvas 960 480)

     pure ()
