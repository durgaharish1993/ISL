module PatternHelpers where
import BasicTypes
import TimeMethods
import TimeSeriesHelpers

import Data.Function (on)
import Data.List (sortBy)




--End of Forall and exist functions. 

applyTimeToBasic :: [(TimeStamp,TimeStamp)] -> [Dir] -> [Pat]
applyTimeToBasic  [] []                = []
applyTimeToBasic  ((t1,t2):ts) (d:ds) = (Bas d t1 t2) : (applyTimeToBasic ts ds)


--this function takes list of Pat and consturcts And Pat. 
applyAndPat  :: [Pat] -> Pat 
applyAndPat  []       =  Bas Up 1 1 
applyAndPat  (x:xs)   =  And x (applyAndPat xs)

--This function takes list of Pat and consturcts Or Pat. 
applyOrPat  :: [Pat] -> Pat 
applyOrPat []      = Bas Up 1 1 
applyOrPat (x:xs)  = Or x (applyOrPat xs)




checkNoPat :: Pat -> Bool
checkNoPat (Bas NoPat t1 t2) = True
checkNoPat p                 = False




getPatInterval :: Pat -> (TimeStamp,TimeStamp)
getPatInterval (Bas d t1 t2)      = (t1,t2)
getPatInterval (Or p1 p2)  
                    |  b1       = (a,b)
                    |  b2       = (c,d)
                    |  otherwise =  ((min4Time a b c d),(max4Time a b c d))
                    where 
                       (a,b) = getPatInterval p2
                       (c,d) = getPatInterval p1
                       b1    = (checkNoPat p1) && (not(checkNoPat p2))
                       b2    = (not(checkNoPat p1)) && (checkNoPat p2)
getPatInterval (And p1 p2)  
                    |  b1       = (a,b)
                    |  b2       = (c,d)
                    |  otherwise =  ((min4Time a b c d),(max4Time a b c d))
                    where 
                       (a,b) = getPatInterval p2
                       (c,d) = getPatInterval p1
                       b1    = (checkNoPat p1) && (not(checkNoPat p2))
                       b2    = (not(checkNoPat p1)) && (checkNoPat p2)

getPatInterval (IfElse c p1 p2)    = if c
                                        then (getPatInterval p1)
                                        else (getPatInterval p2)
                                    



cPatTSeries :: Pat -> TimeSeries a -> [(TimeStamp,a)]
cPatTSeries (Bas NoPat _ _ )   _   = []
cPatTSeries (Bas d t1 t2) xs    =  [ (t1, (xs t1)), (t2, (xs t2)) ]
cPatTSeries (And p1 p2)  xs        = if (not (checkNoPat p1))
                                      then if (not (checkNoPat p2))
                                              then (cPatTSeries p1 xs) ++ (cPatTSeries p2 xs)
                                              else (cPatTSeries p1 xs)
                                      else  (cPatTSeries p2 xs)                                  
cPatTSeries (Or p1 p2)   xs        = if (not (checkNoPat p1))
                                      then if (not (checkNoPat p2))
                                              then   (cPatTSeries p1 xs) ++ (cPatTSeries p2 xs)
                                              else  (cPatTSeries p1 xs)
                                      else   (cPatTSeries p2 xs)
cPatTSeries (IfElse c p1 p2) xs    =  if c
                                      then  (cPatTSeries p1 xs)
                                      else  (cPatTSeries p2 xs)




--patConSeries :: Pat -> TimeSeries a -> [(TimeStamp,a)]
--patConSeries (Bas NoPat _ _ )   _   = []
--patConSeries (Bas Up t1 t2) xs      =  [ (t1, (xs t1)), (t2, (xs t2)) ]
--patConSeries (Bas Down t1 t2) xs    =  [ (t1, (xs t1)), (t2, (xs t2)) ]
--patConSeries (Bas Cons t1 t2) xs    =  [ (t1, (xs t1)), (t2, (xs t2)) ]
--patConSeries (And p1 p2)  xs        = if (not (checkNoPat p1))
--                                      then if (not (checkNoPat p2))
--                                              then (patConSeries p1 xs) ++ (patConSeries p2 xs)
--                                              else (patConSeries p1 xs)
--                                      else  (patConSeries p2 xs)                                  
--patConSeries (Or p1 p2)   xs        = if (not (checkNoPat p1))
--                                      then if (not (checkNoPat p2))
--                                              then   (patConSeries p1 xs) ++ (patConSeries p2 xs)
--                                              else  (patConSeries p1 xs)
--                                      else   (patConSeries p2 xs)
--patConSeries (IfElse c p1 p2) xs    =  if c
--                                      then  (patConSeries p1 xs)
--                                      else  (patConSeries p2 xs)


getUniqueTuples :: (Eq a) =>[(TimeStamp,a)] -> [(TimeStamp,a)]
getUniqueTuples []                              = []
getUniqueTuples (x:xs) 
                  | elem x gx                   = gx
                  | otherwise                   = [x] ++ gx
                  where 
                    gx = (getUniqueTuples xs)


patToSeries ::(Eq a) => Pat -> TimeSeries a -> [(TimeStamp,a)]
patToSeries p x = mySort (getUniqueTuples (cPatTSeries p x))


mySort ::[(TimeStamp, a)] -> [(TimeStamp, a)]
mySort = sortBy ( compare `on` fst)



cPat2TList :: (Eq a) =>[Pat] -> TimeSeries a -> [[(TimeStamp,a)]]
cPat2TList pl x =  map (\y -> (patToSeries y x)) pl

cPat1TList :: [Pat] -> TimeSeries a -> [(TimeStamp,a)]
cPat1TList pl x = concat $ map (\y -> (cPatTSeries y x)) pl










--Plotting 



--  convertTimeSeriesTuple ( cPatTList ( patGenList (exPatBas1) (lift2Match 10  (exPatBas1) openData 470) ))


--[(15,170.2),(16,169.86),(15,170.2),(17,170.05),(15,170.2),(18,168.5),(15,170.2),(19,169.57),(15,170.2),(20,166.5),(15,170.2),(21,165.37),(15,170.2),(22,165.25),(15,170.2),(23,165.28),(15,170.2),(24,167.16),(15,170.2),(25,169.0)]
--[(16,169.86),(17,170.05),(18,168.5),(19,169.57),(20,166.5),(21,165.37),(22,165.25),(23,165.28),(24,167.16),(15,170.2),(25,169.0)]



--[[(16,169.86),(17,170.05),(18,168.5),(19,169.57),(20,166.5),(21,165.37),(22,165.25),(23,165.28),(24,167.16),(15,170.2),(25,169.0)],

--[(11,167.46),(12,167.84),(13,168.59),(14,169.23),(15,170.2),(16,169.86),(17,170.05),(18,168.5),(19,169.57),(10,166.37),(20,166.5)]]








