module Parser where

data BProgram = Program [BDefinition]
              deriving (Eq, Show)

data BDefinition = BDefinition
                 deriving (Eq, Show)

data BIVal = IConstant BConstant
           | IName BName
           deriving (Eq, Show)

data BStatement = Auto     [(BName, BConstant)]
                | Extrn    [BName]
                | Default  [(BConstant, BStatement)] -- TODO: Confirm if this correct
                | Case     [(BConstant, BStatement)]
                | Block    [BStatement]
                | IfElse   (BRValue, BStatement, BStatement)
                | While    (BRValue, BStatement)
                | Switch   (BRValue, BStatement)
                | Goto     BRValue
                | Return   BRValue
                | SRValue BRValue
                deriving (Eq, Show)

data BRValue = BracketRValue BRValue
             | RLValue       BLValue
             | RConstant     BConstant
             | Assignment    (BLValue, BRValue)
             | IncDecPre     (BIncDec, BLValue)
             | IncDecPost    (BIncDec, BLValue)
             | RUnary        (BUnary, BRValue)
             | GetAddress    BLValue
             | Binary        (BRValue, BBinary, BRValue)
             | Ternary       (BRValue, BRValue, BRValue)
             -- TODO
             deriving (Eq, Show)

data BIncDec = Increment
             | Decrement
             deriving (Eq, Show)

data BUnary = Negative
            | Exclamation
            deriving (Eq, Show)

data BBinary = Or
             | And
             | Equal
             | NotEqual
             | LessThan
             | LessThanOrEqual
             | MoreThan
             | MoreThanOrEqual
             | ShiftLeft
             | ShiftRight
             | Add
             | Subtract
             | Modulo
             | Multiply
             | Divide
            deriving (Eq, Show)

data BLValue = LName       BName
             | Dereference BRValue
             | Array       (BRValue, BRValue)     -- TODO: Confirm if this is correct
             deriving (Eq, Show)

data BConstant = Digit Int
               | Chars String
               deriving (Eq, Show)

data BName = Name String
           deriving (Eq, Show)

newtype Parser a = Parser { runParser :: String -> Either (Int, Int, String) (String, a)}
