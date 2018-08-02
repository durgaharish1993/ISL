module PatternAlgebra where



import Data.Generics
import Prelude
import System.IO
import Data.List.Split
import BasicTypes
import PatternHelpers
import CompanyCash
import TimeMethods
import TimeSeriesHelpers




---Semantic Operations of Pattern
-- (-) Pattern   			 	        --> Pat 				      --> Up to Down, Down to Up, Constant to Constant (No Change in TimeInterval)
-- p1 + p2        		 		      --> Pat 				      --> And P1 P2 
-- p1 + Timedelta  				      --> Pat					      --> Shift by Timedelta value. 
-- p1==p2          				      --> Bool 				      --> if all the predicates are same. 
-- slice p1 (TimeInterval)	    --> Pat		    		    --> Slice a pattern
-- normalize P1                 --> Pat				      	--> reduce the pattern to the start poisition 
-- forAll p1.                   --> Pat               --> For all Patterns. 
-- exists p1                    --> Pat.              --> There exits pattern. 
-- discontin p1              	  --> [TimeInterval]		--> Find the discontinunity in a pattern 
-- mergePattern p1              --> Pat 				      --> If there is a discontinuity then merge it. (Comment : this is not implemented) 



-- (-) Pattern --> Pat --> Up to Down, Down to Up, Constant to Constant (No Change in TimeInterval)
patMin :: Pat -> Pat 
patMin (Bas NoPat t1 t2) =  Bas NoPat t1 t2
patMin (Bas Up t1 t2)    =  Bas Down t1 t2
patMin (Bas Down t1 t2)  =  Bas Up t1 t2
patMin (Bas Cons t1 t2)  =  Bas Cons t1 t2 
patMin (And p1 p2)       =  And (patMin p1) (patMin p2)
patMin (Or p1 p2)        =  Or (patMin p1) (patMin p2)
patMin (IfElse c p1 p2)  =  IfElse c (patMin p1) (patMin p2)

-- p1 + p2 --> Pat--> And P1 P2 
patPlus :: Pat -> Pat -> Pat 
patPlus p1 p2                 = And p1 p2 


-- p1 + Timedelta-->Pat--> Shift by Timedelta value. 
---This Shifts the Pattern by TimeDelta Value. 
shiftPattern :: Pat -> TimeDelta-> Pat
shiftPattern (Bas d t1 t2) x       = Bas d (t1+x) (t2+x)
shiftPattern (Or p1 p2) x          = Or (shiftPattern p1 x ) (shiftPattern p2 x)
shiftPattern (And p1 p2) x         = And (shiftPattern p1 x) (shiftPattern p2 x)
shiftPattern (IfElse c p1 p2) x    = IfElse c (shiftPattern p1 x) (shiftPattern p2 x)


--p1==p2 --> Bool--> if all the predicates are same. 
--patEquals exPat1 (shiftPattern exPat1 10)
patEquals :: Pat -> Pat -> Bool
patEquals p1 p2 = (patNorm p1) == (patNorm p2)

-- slice p1 (TimeInterval)--> Pat--> Slice a pattern
patSlice :: Pat -> (TimeStamp,TimeStamp) -> Pat 
patSlice = undefined


----normalize p1 --> Pat--> reduce the pattern to the start poisition 
patNorm :: Pat -> Pat 
patNorm  p =  let 
                (t1,t2)      = getPatInterval p
                td          = _negTimeDelta (_calculateTimeInterval  t1 (_getPatStartTime))
              in
                shiftPattern p (td)

                 
--discontin p1 --> [TimeInterval]--> Find the discontinunity in a pattern 
patFindDis :: Pat -> Pat
patFindDis = undefined


-- mergePattern p1--> Pat--> If there is a discontinuity then merge it. 
patMergeDis :: Pat -> Pat
patMergeDis = undefined


-- This is for Generating List of Patterns 
patGenList :: Pat -> [(TimeStamp,TimeStamp)] -> [Pat]
patGenList p xs =  (map (\x ->(shiftPattern  (patNorm p)  (_calculateTimeInterval (fst x) _getPatStartTime)) ) xs)
--This is one more implementation of the above function. 
--patGenList p ((t1,t2):xs) =  (shiftPattern  (patNorm p)  (_calculateTimeInterval t1 _getPatStartTime)) : (patGenList p xs)







--Here are the functions for Semantic Domain Functions. 


matchPat :: (Ord a,Num a) => Pat -> TimeSeries a -> Bool
matchPat (Bas NoPat _ _ )   _   = True
matchPat (Bas Up t1 t2)   xs    = (xs t2) >= (xs t1)
matchPat (Bas Down t1 t2) xs    = (xs t2) <= (xs t1)
matchPat (Bas Cons t1 t2) xs    = ((xs t2) - (xs t1)) < 1
matchPat (And p1 p2)  xs        = if (not (checkNoPat p1))
                                      then if (not (checkNoPat p2))
                                              then (matchPat p1 xs) && (matchPat p2 xs)
                                              else (matchPat p1 xs)
                                      else  (matchPat p2 xs)                                  
matchPat (Or p1 p2)   xs        = if (not (checkNoPat p1))
                                      then if (not (checkNoPat p2))
                                              then (matchPat p1 xs) || (matchPat p2 xs)
                                              else (matchPat p1 xs)
                                      else  (matchPat p2 xs) 
matchPat (IfElse c p1 p2) xs    =  if c
                                      then (matchPat p1 xs)
                                      else (matchPat p1 xs)

-- And (Bas Up 1 2) (And (Bas Up 1 3) (Bas Up 1 4))
_forAllPat :: TimeStamp -> TimeStamp -> TimeDelta -> TimeDelta -> Dir -> Pat
_forAllPat t1 t2 nc n d =  let 
                          t  = _incrementTime t1 n
                          n1 = (_updateInc t t1) + nc  
                        in if t < t2
                             then And (Bas d t1 t) (_forAllPat t1 t2 nc n1 d)
                             else Bas d t1 t2

_existPat :: TimeStamp -> TimeStamp -> TimeDelta -> TimeDelta -> Dir -> Pat 
_existPat t1 t2 nc n d =  let 
                          t  = _incrementTime t1 n
                          n1 = (_updateInc t t1) + nc  
                        in if t < t2
                             then Or (Bas d t1 t) (_existPat t1 t2 nc n1 d)
                             else Bas d t1 t2
 


forAllPat :: (TimeStamp,TimeStamp) -> TimeDelta -> Dir -> Pat 
forAllPat (t1,t2) n d = _forAllPat t1 t2 n n d 


existPat :: (TimeStamp,TimeStamp) -> TimeDelta -> Dir -> Pat 
existPat (t1,t2) n d = _existPat t1 t2 n n d





----High level Matching 
-- This function matches all the TimeSeries Data. 
--  lift2Match 2 (patMin exPat1 ) openData 490
lift2Match :: (Ord a,Num a) => TimeDelta -> Pat -> TimeSeries a -> TimeStamp -> [(TimeStamp,TimeStamp)]
lift2Match n p se ft = let 
                          nP = (shiftPattern p n)
                       in 
                          if (snd (getPatInterval nP) < ft) 
                              then  if (matchPat nP se)
                                       then (getPatInterval nP):(lift2Match n nP se ft)
                                       else (lift2Match n nP se ft)                              
                              else []



--This modifies the Pattern Time Interval.
modifyTimeIntervals :: Pat -> (TimeStamp,TimeStamp) -> Pat 
modifyTimeIntervals p  (mt1,mt2)= shiftPattern p (_calculateTimeInterval mt1 (fst (getPatInterval p)))



matchP :: Pat -> String -> Bool 
matchP p cs = matchPat p (getCPrice (nComp cs)) 

allTBool :: [Bool] -> Bool 
allTBool []     = True
allTBool (x:xs) = if x
                    then allTBool xs
                    else False

matchAllC :: Pat -> [String] -> Bool
matchAllC p xs = allTBool (map (\x->(matchP p x)) xs)







--------Pattern Examples. 

basicPat :: Pat
basicPat = Bas Up 2 4

basicOrPat :: Pat
basicOrPat = Or (Bas Up 2 4) (Bas Up 4 5)

--This section is defining constants 
listPat1 ::[Pat]
listPat1 = applyTimeToBasic [(1,2),(2,3),(5,6)] [Up,Up,Down]

exPatFor :: Pat
exPatFor = (forAllPat (20,30) 1 Up) 

p1 :: Pat 
p1 = (forAllPat (320,350) 5 (Down))


exPat3 :: Pat
exPat3= And p1 (patMin (shiftPattern p1 31) )  

exProg :: [Prog]
exProg=[(L 1000 2),(UnL 100 3),(B 4 [(2,"CVS"),(1,"Apple")]), (IE ((matchP (shiftPattern (patNorm exPat3) 15)) "Apple")) (B 25 [(2,"Apple")] ) (S 25 [(0.5,"Apple")] ) ]


exPatDis1 :: Pat 
exPatDis1 = And (Bas Up 6 10) (Bas Down 15 20)

exPatOr1 :: Pat 
exPatOr1 =  Or (Bas Up 1 3) (Bas Down 4 7)


exPatIfElse1 :: Bool -> Pat 
exPatIfElse1 = \x->IfElse x exPatFor exPatDis1

exPatBas1 :: Pat
exPatBas1 = (Bas Up 1 10)

exPatOr2 :: Pat
exPatOr2 = Or (Bas Up 1 10) (Bas NoPat 1 100)


exPatComplex :: Pat 
exPatComplex = And (patMin exPatFor) (patMin (shiftPattern (patMin exPatFor) 11))


outPat :: [(TimeStamp,TimeStamp)]
outPat = lift2Match 15 (patNorm exPatComplex) openData 470


--Good Example 
modifyUpInter :: Pat
modifyUpInter = modifyTimeIntervals (patNorm  exPatFor ) (10,20) 

modifyDownInter :: Pat
modifyDownInter = modifyTimeIntervals (patMin (patNorm  exPatFor )) (15,25) 

getListData :: [[(Int,Float)]]
getListData = (cPat2TList [modifyDownInter,modifyUpInter]  openData )


--Example to show how to plot. 
exPatList :: [Pat] 
exPatList = patGenList (exPatBas1) (lift2Match 10  (exPatBas1) openData 470) 

--This is plotting the Patterns 

pltPat1 :: [[(TimeStamp,Price)]]
pltPat1 = cPat2TList exPatList openData

----------------------------
-------- Pattern Examples End
----------------------------
--This is Or Patterns. 


--Examples to Show for presentation 















