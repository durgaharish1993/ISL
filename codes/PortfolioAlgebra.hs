module PortfolioAlgebra where 


import BasicTypes
import TimeSeriesHelpers
import PatternAlgebra
import PatternHelpers
import TimeMethods
import Data.Function (on)
import Data.List (sortBy)
import CompanyCash





--data Prog = Load Cash TimeStamp
--          | UnLoad Cash TimeStamp
--          | Action
--          | IE Bool Prog Prog

--type StockProg = [Prog]


--Semantics for Prog. 
listSemProg :: (Cur,Price,Stocks) -> [Prog] -> (Cur,Price,Stocks)
listSemProg a []       = a
listSemProg a (x:xs)   = listSemProg (calSemProg a x) xs

calSemProg ::  (Cur,Price,Stocks) -> Prog -> (Cur,Price,Stocks)
calSemProg (c,ic,is) (IE b p1 p2)             = if b
                                                 then (calSemProg (c,ic,is) p1)
                                                 else (calSemProg (c,ic,is) p1)
calSemProg (c,ic,is) (B t1 [])                = (c,ic,is)
calSemProg (c,ic,is) (S t1 [])                = (c,ic,is)
calSemProg (c,ic,is) (S t1 ((u,co):xs))       = let 
                                                  (uC,uS)=(sellStock ((_pToCa ic c),is) (Sell t1 u ( nComp co))) 
                                                in  
                                                  calSemProg (c,(_cCashtoPrice uC),uS) (S t1 xs)
calSemProg (c,ic,is) (B t1 ((u,co):xs))       = let
                                                  (uC,uS)=  (buyStock  ((_pToCa ic c),is) (Buy  t1 u ( nComp co))) 
                                                in
                                                  calSemProg (c,(_cCashtoPrice uC),uS) (B t1 xs)
calSemProg (c,ic,is) (L c1 t1)                = (c,ic+c1,is)
calSemProg (c,ic,is) (UnL p2 t1)              = if p2<ic
                                                then (c,(ic-p2),is)
                                                else (c,(ic),is)



_pToCa :: Price -> Cur -> Cash
_pToCa a d = (C d a) 







--Example of PortFolio

exPF1 :: Stocks
exPF1 = Scale 10 (Stock 1 C1)

exPF2 :: Stocks 
exPF2 = A (Scale 10 (A (Stock 1 C1) (Stock 1 C2) ) ) (A (Stock 1 C1) (Stock 2 C2))

exListStocks :: [Stocks]
exListStocks = [Stock 1 C1, Scale 10 (Stock 3 C1), Scale 2 (Stock 10 C1)]

exPF3 :: Stocks 
exPF3 =  A  (A (Scale 10 (Stock 3 C1))  (Scale 10  (A (Stock 1 C1) (Stock 1 C2)))) (Scale 2 (Stock 10 C1))

exPF4 :: Stocks 
exPF4 = Scale 10 (Stock 1 C2)

--This is for Company to Curreny Type. 




--data Portfolio = Zero
--               | Stock TimeStamp Comp
--               | Scale Float Portfolio
--               | A Portfolio Portfolio



scaleCash :: Cash -> Float -> Cash 
scaleCash (C d p) unit = C d (unit*p)  

getPortTUP :: Stocks -> TimeStamp -> Cur -> [(Comp,Cash)]
getPortTUP   Zero          _ _ = []
getPortTUP  (Stock t1 c1)  t c = [(c1, getCompanyTSeries c1 t)]
getPortTUP  (Scale unit p) t c = let 
                                   a = (getPortTUP p t c) 
                                 in 
                                   map (\x-> ((fst x),(scaleCash (snd x) unit))) a

getPortTUP (A p1 p2) t c       = (getPortTUP p1 t c) ++ (getPortTUP p2 t c) 



_cCashtoPrice :: Cash -> Price 
_cCashtoPrice  (C d p) =  p

_cToCur :: Cash -> Cur
_cToCur (C d p)  = d 


--I Need to handle when Cashes are in different Currency.
_diffCash :: Cash -> Cash -> Cash
_diffCash (C d c1) (C d1 c2)  = if(d==d1)
                                   then C d (c1 - c2)
                                   else C d (c1 -c2)


_addCash :: Cash -> Cash -> Cash
_addCash (C d c1) (C d1 c2)  = if(d==d1)
                                   then C d (c1 + c2)
                                   else C d (c1 + c2)       

--When the Stock are Unitary.
--It does not work for And Cases
getTimeBuy :: Stocks -> TimeStamp
getTimeBuy (Zero)      = 1
getTimeBuy (Stock t c) = t 
getTimeBuy (Scale _ s) = (getTimeBuy s)




portFTotalCash :: Stocks -> TimeStamp -> Cur -> Cash
portFTotalCash p t cu = C cu (sum ( map (\x -> _cCashtoPrice (snd x))  (getPortTUP p t cu) ))


checkBuyPower :: Cash -> Action -> (Bool,Cash)
checkBuyPower (C d v) (Buy t unit c) = let 
                                         a  = (portFTotalCash (Scale unit (Stock t c)) t d)
                                         cv = v - (_cCashtoPrice a)  
                                       in 
                                         if (cv)>0
                                            then (True, C d cv)
                                            else (False,C d v)



buyStock :: (Cash,Stocks) -> Action -> (Cash,Stocks)
buyStock (ca,s) (Buy t unit c) = let 
                                   (b,nc)=(checkBuyPower ca (Buy t unit c))
                                  in 
                                   if b
                                      then (nc,(A s (Scale unit (Stock t c))))
                                      else (nc,s)


--This will find the Stocks and returns as units.
findCompStock :: Stocks -> Comp -> [Stocks]
findCompStock (Zero)       c = []
findCompStock (Stock t c1) c = if(c==c1)
                                  then [Stock t c1]
                                  else []
findCompStock (A s1 s2)    c = (findCompStock s1 c) ++ (findCompStock s2 c)
findCompStock (Scale u s)  c = map (\x -> Scale u x) (findCompStock s c)

--Good Work
removeCompStock :: Stocks -> Comp -> Stocks
removeCompStock Zero c          = Zero 
removeCompStock (Stock t c1) c  = if(c==c1)
                                      then Zero
                                      else (Stock t c1)
removeCompStock (A s1 s2)    c  = A (removeCompStock s1 c) (removeCompStock s2 c)
removeCompStock (Scale u s)  c  = Scale u (removeCompStock s c)


normStock :: Stocks -> Stocks
normStock Zero        = Zero
normStock (Scale u s) = s 
normStock (Stock t c) = (Stock t c)

getSSize :: Stocks -> Float
getSSize (Stock t c) = 1
getSSize (Scale u s) = u

thr :: (a,b,c) -> c
thr (_,_,x) = x

sortByCash ::[((Float, Stocks), Cash)] -> Cur -> [( (Float,Stocks),Cash)]
sortByCash (xs) c = map (\x-> ((fst x), (C c (snd x) )) ) (sortBy (flip ( compare `on` snd)) ( map (\x -> ( fst x, (_cCashtoPrice (snd x))) ) xs))




updateStocks :: Stocks -> [Stocks] -> Stocks
updateStocks s []     = s
updateStocks s (x:xs) = let 
                          ns =  (A s x) 
                          os =  (updateStocks ns xs)
                        in 
                          os 

--This function Helps for bSellMaxProfit Function(Note : it takes only Sorted List )
--Outputs , Cash Obatained by selling and remaning Stocks to Add.  
--_conNewStocks :: [((Float,Stocks),Cash)]  -> Action -> Cash -> (Cash,[Stocks])

_conNewStocks :: [((Float,Stocks),Cash)]  -> Action -> Cash -> [Stocks]-> (Cash,[Stocks])
_conNewStocks   []   a  ca  sca                      =  (ca,sca)
_conNewStocks  (((u,s),_):xs) (Sell t unit c) ca sca =  if (u-unit) <0
                                                        then  let 
                                                                ca_o = (portFTotalCash (Scale u (normStock s)) t (getCurType c))
                                                                ca_or = _addCash ca_o ca
                                                              in 
                                                                (_conNewStocks xs (Sell t (unit-u) c) ca_or sca)
                                                        else let 
                                                               ca_o1 =  (portFTotalCash (Scale unit (normStock s)) t (getCurType c))
                                                               ca_o2 = _addCash ca_o1 ca
                                                             in 
                                                               if (u-unit) ==0
                                                                  then (_conNewStocks xs (Sell t 0 c) ca_o2 sca )
                                                                  else (_conNewStocks xs (Sell t 0 c) ca_o2 (sca ++ [Scale (u-unit) (normStock s) ])) 
                                                               




test1 ::(Cash,[Stocks])
test1 = bSellMaxProfit exListStocks (Sell 225 10 C1)

--This is a Optimization Problem, You can come up with a Stategy
--I will use a Greedy Approach to sell the Stock 
bSellMaxProfit :: [Stocks] -> Action -> (Cash,[Stocks])
bSellMaxProfit xs  (Sell t unit c) = let
                                        --[((Float,Stocks),Cash)] (units,Stocks),CashDiffernce
                                        cl = map (\x ->((getSSize x, x),(_diffCash (portFTotalCash (normStock x) t (getCurType c)) (portFTotalCash (normStock x) (getTimeBuy x) (getCurType c))))) xs
                                        scl = (sortByCash cl (getCurType c) )
                                        uS  = _conNewStocks scl (Sell t unit c) (C (getCurType c) 0) []
                                       in 
                                        uS
                                          

sellStock :: (Cash,Stocks) -> Action -> (Cash,Stocks)
sellStock (ca,s) (Sell t unit c) = let 
                                     xs = (findCompStock s c)
                                   in 
                                     if xs ==[]
                                        then (C (getCurType c) 0,s)
                                        else let 
                                                (ob,ns) = (bSellMaxProfit xs (Sell t unit c))
                                                tempP   = (removeCompStock s c)
                                                updaS   = (updateStocks tempP ns)
                                             in
                                                (_addCash ca ob, updaS)



exPF6 :: Stocks
exPF6 = Scale 10 (Stock 1 C1)

sellEx1  :: (Cash,Stocks)
sellEx1  =  sellStock (C USD 0, exPF3) (Sell 20 2 C1)

exPF5 :: Cash
exPF5 = portFTotalCash (snd sellEx1) 20 USD




---Examples to Show for presentation
exProg1 :: (Cur,Price,Stocks)
exProg1 = listSemProg (USD,1000,Zero) [(L 1000 2),(UnL 100 3),(B 4 [(2,"CVS"),(1,"Apple")]),(IE (matchP exPatFor "Apple") (UnL 500 40) (B 40 [(2,"Apple")] ))]


exProg2 :: (Cur,Price,Stocks)
exProg2 = listSemProg exProg1 [(L 2000 35), IE (matchAllC  (shiftPattern (patNorm exPatDis1) 50) ["Apple","Cummins","Amazon.com"]) (B 70 [(2,"Apple"),(1.2,"Cummins"),(1,"Amazon.com")]) (S 70 [(1,"Apple")])  ]












































