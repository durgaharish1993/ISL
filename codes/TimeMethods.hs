module TimeMethods where 



import BasicTypes




--These are helper function for doing exist and forall patterns. 
_incrementTime :: TimeStamp -> TimeDelta -> TimeStamp
_incrementTime x n = x+n 



_getPatStartTime :: TimeStamp
_getPatStartTime = 1

_negTimeDelta :: TimeDelta -> TimeDelta
_negTimeDelta t1 = -t1 

_updateInc :: TimeStamp -> TimeDelta -> TimeStamp
_updateInc x y = x-y 


_calculateTimeInterval :: TimeStamp -> TimeStamp -> TimeDelta
_calculateTimeInterval x y = x-y


minTime :: TimeStamp -> TimeStamp -> TimeStamp
minTime x y = (min x y) 

min4Time :: TimeStamp -> TimeStamp -> TimeStamp -> TimeStamp -> TimeStamp
min4Time p q r s = minTime (minTime (minTime p q) r) s

maxTime :: TimeStamp -> TimeStamp -> TimeStamp
maxTime x y = (max x y) 

max4Time :: TimeStamp -> TimeStamp -> TimeStamp -> TimeStamp -> TimeStamp
max4Time p q r s = maxTime (maxTime (maxTime p q) r) s



