-- Exercise 2, Pascal's triangle

{-# LANGUAGE RankNTypes #-}

import Control.Exception
import qualified Graphics.UI.Gtk          as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Builder  as B

type X               = Double
type Y               = Double
type Side            = Double
type AmountOfSquares = Double
type NumberFloor     = Double
type AmountOfFloors  = Double
type Floor           = (NumberFloor, ValuesFloor)
type ValuesFloor     = [Double]
type PascalTriangle  = [(NumberFloor, ValuesFloor)]

main :: IO ()
main = do
   G.initGUI

   builder <- B.builderNew
   B.builderAddFromFile builder "Ex2.glade"
 
   let get :: forall cls . G.GObjectClass cls
           => (G.GObject -> cls)
           -> String
           -> IO cls
       get = B.builderGetObject builder

   win        <- get G.castToWindow      "main_window"
   buttDraw   <- get G.castToButton      "button_draw"
   labError   <- get G.castToLabel       "label_error"
   entrFloors <- get G.castToEntry       "entry_floors"
   drawArea   <- get G.castToDrawingArea "drawing_area"

   G.onDestroy win G.mainQuit
   G.onClicked buttDraw $ do
      amountFloors <- G.entryGetText        entrFloors
      drawWindow   <- G.widgetGetDrawWindow drawArea
      (wid,hei)    <- G.drawableGetSize     drawWindow

      if all (\x -> x `elem` "0123456789") amountFloors
      then  
       (do let amFl      = read amountFloors :: Double
               windSide  = realToFrac wid
               side      = (realToFrac wid) / amFl
               pascalTr  = setPascalTriangle amFl
           G.renderWithDrawable drawWindow $ do
              C.setSourceRGBA 0 0 0 0
              C.setOperator   C.OperatorSource
              C.paint
              C.setSourceRGB  0 1 0
              C.setLineCap    C.LineCapRound
              C.setLineJoin   C.LineJoinRound
              drawPyramid 0 (windSide-side) side amFl pascalTr
           G.labelSetText labError "OK" )
      else
         (do G.renderWithDrawable drawWindow $ do
                C.setSourceRGBA 0 0 0 0
                C.setOperator   C.OperatorSource
                C.paint
             G.labelSetText labError
                "ERROR!!! PLEASE ENTER AN INTEGER NUMBER!! ERROR!!!")
   G.widgetShowAll win

   G.mainGUI

---- Functions

------ Dirty

drawPyramid :: X
            -> Y
            -> Side
            -> AmountOfFloors
            -> PascalTriangle
            -> C.Render ()
drawPyramid _ _ _    floors _ | floors <= 0 = C.stroke
drawPyramid x y side floors pasTr  = do
   drawSquares x y side floors currentFloor
   drawPyramid (x+side/2) (y-side) side (floors-1) pasTr
      where 
      currentFloor = head $ filter (\(x,y) -> x  == floors) pasTr

drawSquares :: X
            -> Y
            -> Side
            -> AmountOfSquares
            -> Floor
            -> C.Render ()
drawSquares _ _ _    n _ | n <= 0 = C.stroke
drawSquares x y side n floor = do
   C.moveTo (x+side/10) (y+side/2)
   C.setFontSize (side/5)
   C.showText  (showValue n floor) 
   C.rectangle x y side side
   C.stroke
   drawSquares (x+side) y side (n-1) floor

------ Pure

setPascalTriangle :: AmountOfFloors -> PascalTriangle
setPascalTriangle = bar root
   where root = (1,[1])
         bar :: Floor -> NumberFloor -> PascalTriangle
         bar _ 0      = [] 
         bar r floors = r : bar (nextFloor r) (floors-1) 

nextFloor :: Floor -> Floor  
nextFloor (num, val) = (num+1, 1 : foo val)
   where
      foo :: Num a => [a] -> [a]
      foo []    = []
      foo [x]   = [x]
      foo (x:s) = (x + head s) : foo s

showValue :: NumberFloor -> Floor -> String
showValue n floor  = 
   show $ round $ (snd $ floor) !! ((round n)-1) 
