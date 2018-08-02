module ISLExamples where 

import BasicTypes
import PatternHelpers
import TimeMethods
import TimeSeriesHelpers
import PatternAlgebra
----------------------------
-------- Pattern Examples Start
----------------------------
--This is Or Patterns. 
basicPat :: Pat
basicPat = Bas Up 2 4

basicOrPat :: Pat
basicOrPat = Or (Bas Up 2 4) (Bas Up 4 5)

--This section is defining constants 
listPat1 ::[Pat]
listPat1 = applyTimeToBasic [(1,2),(2,3),(5,6)] [Up,Up,Down]

exPatFor :: Pat
exPatFor = (forAllPat (10,20) 1 Up) 

exPatAll :: Pat 
exPatAll = And exPatFor (shiftPattern (patNorm (patMin exPatFor)) 21)

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


