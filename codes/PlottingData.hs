--module PlottingData where 

--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Cairo
--import Data.Time
--import Graphics.Rendering.Chart.Axis.Floating
--import Graphics.Rendering.Chart.Axis.Int


----These are the modules from 
--import BasicTypes
--import TimeMethods
--import TimeSeriesHelpers


-----------End of Imports...

--type PlotVal = (EC (Layout Int Float) ()) 
--type DataTS  =[(Int,Float)]



--------------------
--plotGraph :: PlotVal  -> String -> IO()
--plotGraph x y  = toFile def y x

--getPlotFun ::  DataTS ->  PlotVal
--getPlotFun x = plot (line "lines" [x])


--getPlotMulFun ::  [DataTS] -> PlotVal 
--getPlotMulFun (x:xs) 
--           | xs ==[]    =  p
--           | otherwise  =  (_helpconti p xs)
--           where
--            p = (getPlotFun x)

--_helpconti :: PlotVal  -> [DataTS] -> PlotVal
--_helpconti p []     = p >>= return
--_helpconti p (x:xs) = p >> (_helpconti (getPlotFun x) xs)


--drawGraph :: [DataTS] -> IO()
--drawGraph xs = plotGraph (getPlotMulFun xs ) "test.png"



------------------------
----Not So Important but, You can understand by looking at this.   
------------------------

----fun1 :: [Double] -> [(Double,Double)]
----fun1  xs =  (map (\x ->(x,x+1) ) xs)


----y1 :: [(Double,Double)]
----y1 = fun1 [0.0,0.5,1.0]

----y2 :: [(Double,Double)]
----y2 = fun1 [2.0,2.5,3.0]


----signal :: [Double] -> [(Double,Double)]
----signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]


----plotSingle :: [(Int,Float)] -> IO()
----plotSingle = \x-> (toFile def "test.png"  (plot (line "lines" [x])) ) 


----plotMToFile :: [(Double,Double)]-> [(Double,Double)] -> IO()
----plotMToFile = \x -> (\y -> toFile def "test.png" $ do
----                                plot (line "lines" [x])
----                                plot (line "lines" [y]))  



----plotMToFile1 :: [(Double,Double)]-> [(Double,Double)] -> IO()
----plotMToFile1 = \x -> (\y -> toFile def "test.png"  ((plot (line "lines" [x]) >> (plot (line "lines" [y])))))  



