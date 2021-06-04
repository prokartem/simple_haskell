-- Exercise 1, Squares with circles in them

{-# LANGUAGE RankNTypes #-}

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Entry.Entry

main = do
   initGUI

   builder <- builderNew
   builderAddFromFile builder "ex1.glade"
   
   let getObj :: forall cls . GObjectClass cls
              => (GObject -> cls)
              -> String
              -> IO cls
       getObj = builderGetObject builder

   win <- getObj castToWindow "main_window"
   onDestroy win mainQuit

   buttDraw    <- getObj castToButton "button_draw"
   buttReset   <- getObj castToButton "button_reset" 
   drawingArea <- getObj castToDrawingArea "drawing_area"
   entrySquares <- getObj castToEntry "entry_squares"  
   entryCircles <- getObj castToEntry "entry_circles"
   
   onClicked buttDraw $ do
      amountCir <- entryGetText entryCircles :: IO String
      amountSq <- entryGetText entrySquares :: IO String
      drawWindow <- widgetGetDrawWindow drawingArea
      (w',h')    <- drawableGetSize drawWindow
      let amSq   = read amountSq :: Double
          amCir  = read amountCir :: Double          
          width  = (realToFrac w') / amSq
          height = (realToFrac h') / amSq
      renderWithDrawable drawWindow $ do
         C.setSourceRGBA 0 0 0 0
         C.setOperator C.OperatorSource
         C.paint
         C.setSourceRGB 0 1 0
         C.setLineCap  C.LineCapRound
         C.setLineJoin C.LineJoinRound
         drawSquaresWithCircles 0 0 width height amSq amCir
   
   onClicked buttReset $ do
      drawWindow <- widgetGetDrawWindow drawingArea
      renderWithDrawable drawWindow $ do
         C.setSourceRGBA 0 0 0 0
         C.setOperator C.OperatorSource
         C.paint
      entrySetText entrySquares ""
      entrySetText entryCircles ""
   --drawingArea `on` exposeEvent $ updateDrawingArea amSq amCir

   widgetShowAll win

   mainGUI

updateDrawingArea :: Double -> Double -> EventM EExpose Bool
updateDrawingArea squares circles = do
   wind <- eventWindow
   C.liftIO $ do
   (width',height') <- drawableGetSize wind
   let width  = (realToFrac width') /squares
       height = (realToFrac height')/squares

   renderWithDrawable wind $ do
      C.setSourceRGBA 1 1 1 1 
      C.setOperator C.OperatorSource
      C.paint
      C.setSourceRGB 0 1 0
      C.setLineCap C.LineCapRound
      C.setLineJoin C.LineJoinRound
      drawSquaresWithCircles 0 0 width height squares circles

   return True

type Width           = Double
type Height          = Double
type AmountOfCircles = Double
type AmountOfSquares = Double
type Radius          = Double
type X               = Double
type Y               = Double

drawSquaresWithCircles :: X
                       -> Y
                       -> Width
                       -> Height
                       -> AmountOfSquares
                       -> AmountOfCircles 
                       -> C.Render ()
drawSquaresWithCircles _ _ _ _ squares _ | squares <=0  = C.stroke
drawSquaresWithCircles x y width height squares circles = do
   C.rectangle x y width height 
   C.stroke
   drawCircles (x+width/2) (y+height/2) radius circles 
   drawSquaresWithCircles (x+width) (y+height) width height (squares-1) circles
   where radius = (min width height)/(2*circles)

drawCircles :: X 
            -> Y 
            -> Radius 
            -> AmountOfCircles 
            -> C.Render ()
drawCircles _ _ _ n | n<=0 = C.stroke
drawCircles x y radius n   = do
   C.arc x y (radius*n) 0 (2*pi)
   C.stroke
   drawCircles x y radius (n-1)
