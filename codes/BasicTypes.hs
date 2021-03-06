module BasicTypes where 

--This module is for defining Open Close Data. 



--These are the primitive Type 
type TimeStamp = Int 
type TimeDelta = Int 
type Price     = Float
type Volume    = Int
type Cond      = Bool
type TimeSeries a = TimeStamp -> a

data Dir = Up | Down | Cons | NoPat
        deriving(Show,Eq)

data Pat = Bas Dir TimeStamp TimeStamp
          | And Pat Pat
          | Or Pat Pat 
          | IfElse Cond Pat Pat
       deriving(Show,Eq)





--Here we define Portfolio 

data Cur  = USD | INR | EURO
      deriving(Show,Eq)

data Comp = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | C11 | C12 | C13 | C14 | C15 | C16 | C17 | C18 | C19 | C20 | C21 | C22 | C23 | C24 | C25 | C26 | C27 | C28 | C29 | C30 | C31 | C32 | C33 | C34 | C35 | C36 | C37 | C38 | C39 | C40 | C41 | C42 | C43 | C44 | C45 | C46 | C47 | C48 | C49 | C50 | C51 | C52 | C53 | C54 | C55 | C56 | C57 | C58 | C59 | C60 | C61 | C62 | C63 | C64 | C65 | C66 | C67 | C68 | C69 | C70 | C71 | C72 | C73 | C74 | C75 | C76 | C77 | C78 | C79 | C80 | C81 | C82 | C83 | C84 | C85 | C86 | C87 | C88 | C89 | C90 | C91 | C92 | C93 | C94 | C95 | C96 | C97 | C98 | C99 | C100 | C101 | C102 | C103 | C104 | C105 | C106 | C107 | C108 | C109 | C110 | C111 | C112 | C113 | C114 | C115 | C116 | C117
      deriving(Show,Eq)


data Stocks = Zero
               | Stock TimeStamp Comp
               | Scale Float Stocks
               | A Stocks Stocks
         deriving(Show,Eq)

type Obs a = TimeStamp -> a  

data Cash      = C Cur Price
       deriving(Show,Eq)


type MonadState = (Cash,Stocks)

data Action = Buy TimeStamp Float Comp 
            | Sell TimeStamp Float Comp





data Prog = L Float TimeStamp
          | UnL Float TimeStamp
          | B TimeStamp [(Float,String)]
          | S TimeStamp [(Float,String)]
          | IE Bool Prog Prog
      deriving(Show,Eq)

type StockProg = [Prog]






--Here we define Portfolio 


--Defining Deep DSL. 


--data Lang = Load (Cash,Stocks)
--          |  






















----The Close Data as Deep embedding form 
--closeDataDeep :: [(TimeStamp,Price)]
----loseDataDeep = [(1,10.1),(2,10.22),(3,20.12),(4,50.3)]
--closeData :: Int -> Double
--closeData = \t -> snd (filter (\p -> (fst p)==t) [(1,10.1),(2,10.22),(3,20.12),(4,50.3)])





--closeData = \t -> fromJust . lookup t $ [(1,10.1),(2,10.22),(3,20.12),(4,50.3)]
