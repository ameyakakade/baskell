module BParser where

type BProgram = [BDefinition]

data BDefinition = FDefinition { fName :: BName, fArgs :: [BName], fStatement :: BStatement }
                 | GlobalVar { vName :: BName, vSize :: Maybe Int, vInit :: [BIVal] }
                 | NakedFunction { nfName :: BName, nfAsm :: [String] }
                 | VariadicFunction { vfName :: BName, vfMinArgs :: Int }
                 deriving (Eq, Show)

data BIVal = IConstant BConstant
           | IName BName
           deriving (Eq, Show)

data BStatement = Auto      [(BName, Maybe Int)]
                | Extrn     [BName]
                | BLabel    BName BStatement
                | Case      Int BConstant BStatement
                | Block     [BStatement]
                | IfElse    BRValue BStatement (Maybe BStatement)
                | While     BRValue BStatement
                | Switch    BRValue BStatement
                | Goto      BRValue
                | BReturn   (Maybe BRValue)
                | SRValue   BRValue
                | InlineAsm [String]
                | Empty
                deriving (Eq, Show)

data BRValue = BracketRValue BRValue
             | RLValue       BLValue
             | RConstant     BConstant
             | Assignment    BLValue BAssign BRValue
             | IncDecPre     BIncDec BLValue
             | IncDecPost    BLValue BIncDec
             | RUnary        BUnary BRValue
             | GetAddress    BLValue
             | Binary        BRValue BBinary BRValue
             | Ternary       BRValue BRValue BRValue
             | FunctionCall  BRValue [BRValue]
             deriving (Eq, Show)

          -- left binding power, right binding power
bindingPower :: BBinary -> (Int, Int)
bindingPower b = case b of
                   Add             -> (3, 4)
                   Subtract        -> (3, 4)
                   Multiply        -> (5, 6)
                   Divide          -> (5, 6)
                   Modulo          -> (5, 6)
                   Equal           -> (1, 2)
                   NotEqual        -> (1, 2)
                   Or              -> (0, 1)
                   And             -> (0, 1)
                   LessThanOrEqual -> (1, 2)
                   LessThan        -> (1, 2)
                   MoreThanOrEqual -> (1, 2)
                   MoreThan        -> (1, 2)
                   ShiftLeft       -> (1, 2)
                   ShiftRight      -> (1, 2)

data BAssign = Assign
             | BinaryAssign BBinary
             deriving (Eq, Show)

data BIncDec = Increment
             | Decrement
             deriving (Eq, Show)

data BUnary = Negative
            | Not
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
             | QuestionMark
             deriving (Eq, Show)

data BLValue = LName       BName
             | Dereference BRValue
             | Array       BRValue BRValue
             deriving (Eq, Show)

data BConstant = Digit       Int
               | HexConst    String
               | OctalConst  String
               | BinaryConst String
               | CharConst   Char
               | Chars       String
               deriving (Eq, Show)

data BName = BName { name :: String, nameLoc :: Int }
           deriving (Eq, Show)

keywords = ["auto", "extrn", "goto", "if", "else", "return", "switch", "case", "__asm__"]


bProgram = undefined

startParser = undefined
