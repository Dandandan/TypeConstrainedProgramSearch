module TCPS where

import Data.Maybe (catMaybes)
import Data.Int (Int32)
import Data.Bits ((.&.), (.|.), xor, popCount, complement)

data Types = Int32Type | Float32Type deriving Show

type StackType = [Types]

data Instruction
    = Int32Add -- | Integer32 Addition
    | Int32Mul -- | Integer32 Multiplication
    | Int32Sub -- | Integer32 Substraction
    | Int32Div -- | Integer32 Division
    | Int32Mod -- | Integer32 Modulus
    | Int32Neg -- | Integer32 Negation
    | Int32Inc -- | Integer32 Increment
    | Int32Dec -- | Integer32 Decrement
    | Int32And -- | Integer32 AND
    | Int32Or -- | Integer32 OR
    | Int32Xor -- | Integer32 XOR
    | Int32PopCount -- | Integer32 PopCount
    | Int32Not -- | Integer32 One's Complement / Not
    | Int32Push Int32 -- | Integer32 Push constant to stack
    | Float32Push Float -- | Integer32 Push constant to stack
    | Float32Add -- | Float32 Addition
    | Float32Mul -- | Float32 Multiplication
    | Float32Sub -- | Float32 Substraction
    | Float32Div -- | Float32 Division
    | Float32Neg -- | Integer32 Negation

    | Lds Int -- | Load value relative to stack pointer
    | Sts Int -- | Store value relative to stack pointer
    -- | TODO: Add much more instructions (floating point, logical), make custom instructions / types possible
    deriving (Show, Eq)

type Program = [Instruction]

data Val = Int32Val Int32 | Float32Val Float | Error deriving (Show, Eq)

type Stack = [Val]

type CPUState = Int -- Program Counter TODO: add stack pointer etc.

-- | Beginning state of CPU 
zeroState :: CPUState
zeroState = 0

-- Executes program given program code, stack and state (emulator)
exec :: Program -> Program -> Stack -> CPUState -> Val
exec program todo stack counter  =
    case (todo, stack) of
        ([], [x]) ->
            x

        (Int32Push i: xs, ys) ->
            exec program xs (Int32Val i: ys) (counter + 1)

        (Int32Add: xs, (Int32Val y1:Int32Val y2: ys)) ->
            exec program xs (Int32Val (y1 + y2): ys) (counter + 1)

        (Int32Sub: xs, (Int32Val y1:Int32Val y2: ys)) ->
            exec program xs (Int32Val (y1 - y2): ys) (counter + 1)

        (Int32Mul: xs, (Int32Val y1:Int32Val y2: ys)) ->
            exec program xs (Int32Val (y1 * y2): ys) (counter + 1)

        (Int32Div: xs, (Int32Val y1:Int32Val y2: ys)) ->
            if y2 /= 0 then
                exec program xs (Int32Val (y1 `div` y2): ys) (counter + 1)
            else
                Error

        (Int32Mod: xs, (Int32Val y1:Int32Val y2: ys)) ->
            if y2 /= 0 then
                exec program xs (Int32Val (y1 `mod` y2): ys) (counter + 1)
            else
                Error

        (Int32Neg: xs, (Int32Val y1: ys)) ->
            exec program xs (Int32Val (-y1): ys) (counter + 1)

        (Int32Inc: xs, (Int32Val y: ys)) ->
            exec program xs (Int32Val (y + 1): ys) (counter + 1)
 
        (Int32Dec: xs, (Int32Val y: ys)) ->
            exec program xs (Int32Val (y - 1): ys) (counter + 1)

        (Int32And: xs, (Int32Val y1: Int32Val y2: ys)) ->
            exec program xs (Int32Val (y1 .&. y2): ys) (counter + 1)

        (Int32Or: xs, (Int32Val y1: Int32Val y2: ys)) ->
            exec program xs (Int32Val (y1 .|. y2): ys) (counter + 1)

        (Int32Xor: xs, (Int32Val y1: Int32Val y2: ys)) ->
            exec program xs (Int32Val (y1 `xor` y2): ys) (counter + 1)

        (Int32PopCount: xs, (Int32Val y: ys)) ->
            exec program xs (Int32Val (fromIntegral $ popCount y): ys) (counter + 1)

        (Int32Not: xs, (Int32Val y: ys)) ->
            exec program xs (Int32Val (complement y): ys) (counter + 1)

        (Float32Push i: xs, ys) ->
            exec program xs (Float32Val i: ys) (counter + 1)

        (Float32Add: xs, (Float32Val y1: Float32Val y2: ys)) ->
            exec program xs (Float32Val (y1 + y2): ys) (counter + 1)

        (Float32Mul: xs, (Float32Val y1: Float32Val y2: ys)) ->
            exec program xs (Float32Val (y1 * y2): ys) (counter + 1)

        (Float32Sub: xs, (Float32Val y1: Float32Val y2: ys)) ->
            exec program xs (Float32Val (y1 - y2): ys) (counter + 1)

        (Float32Div: xs, (Float32Val y1: Float32Val y2: ys)) ->
            exec program xs (Float32Val (y1 / y2): ys) (counter + 1)
    
        (Float32Neg: xs, (Float32Val y1: ys)) ->
            exec program xs (Float32Val (-y1) : ys) (counter + 1)

        (Lds i: xs, ys) ->
            exec program xs (ys !! (abs i - 1): ys) (counter + 1)

        (Sts i: xs, y:ys) ->
            let
                (z,_:zs) = splitAt (abs i - 2) ys
                new = z ++ [y] ++ zs
            in
                exec program xs new (counter + 1)
        _ ->
            error "exec: this should not have happened"

-- | Gets StackType given values on stack
getTypeOfStack :: Stack -> StackType
getTypeOfStack (Int32Val _: xs) = Int32Type: getTypeOfStack xs
getTypeOfStack (Float32Val _: xs) = Float32Type: getTypeOfStack xs
getTypeOfStack _ = []

-- Whether the current type is of one singular value
isResult :: StackType -> Bool
isResult (Int32Type:[]) = True
isResult (Float32Type:[]) = True
isResult _ = False

-- | Finds whether there are identity parts in the program to
-- | reduce the search space.
-- | Also rejects inputs like division by zero
isIdentityOrNotOk :: Program -> Bool
isIdentityOrNotOk program = 
    case program of
        -- x + 0 = x, 0 + x = x
        (Int32Add: Int32Push 0: _) -> True
        (Int32Add: Int32Push _: Int32Push 0: _) -> True

        -- x * 1 = x, 1 * x = x
        (Int32Mul: Int32Push 1: _) -> True
        (Int32Mul: Int32Push _: Int32Push 1: _) -> True

        -- x - 0 = x
        (Int32Sub: Int32Push 0: _) -> True

        -- x / 1 = x
        (Int32Div: Int32Push 1: _) -> True

        -- -0 = 0
        (Int32Neg: Int32Push 0: _) -> True

        -- x AND 0 == 0, 0 AND x == 0
        (Int32And: Int32Push 0: _) -> True
        (Int32And: Int32Push _: Int32Push 0: _) -> True

        -- x OR 0 == x, 0 or x == x
        (Int32Or: Int32Push 0: _) -> True
        (Int32Or: Int32Push _: Int32Push 0: _) -> True

        -- popCnt 0 == 0
        (Int32PopCount: Int32Push 0: _) -> True
        -- popCnt 1 == 1
        (Int32PopCount: Int32Push 1: _) -> True

        -- x % 0 = crash
        (Int32Mod: Int32Push 0: _) -> True
        -- x / 0 = crash
        (Int32Div: Int32Push 0: _) -> True
        -- TODO: add more here (or search for them)
        _ -> False

-- Computes type of program if type correct
typeOf :: Instruction -> StackType -> Maybe StackType
typeOf instr stacktype =
    case (instr, stacktype) of
        (Int32Add, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Mul, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Sub, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Div, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Mod, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Neg, Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Inc, Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Dec, Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32And, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Or, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Xor, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Not, Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32PopCount, Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Push _, ys) ->
            Just(Int32Type: ys)
        (Float32Push _, ys) ->
            Just(Float32Type: ys)
        (Float32Add, Float32Type: Float32Type: ys) ->
            Just(Float32Type: ys)
        (Float32Mul, Float32Type: Float32Type: ys) ->
            Just(Float32Type: ys)
        (Float32Sub, Float32Type: Float32Type: ys) ->
            Just(Float32Type: ys)
        (Float32Div, Float32Type: Float32Type: ys) ->
            Just(Float32Type: ys)
        (Float32Neg, Float32Type: ys) ->
            Just(Float32Type: ys)
        (Lds i, ys) ->
            if i < 0 && length ys >= abs i then
                Just (ys !! (abs i - 1): ys)
            else
                Nothing
        (Sts i, y: ys) ->
            if i < -1 && length ys >= abs i - 1 then
                let
                    (xs,_:zs) = splitAt (abs i - 2) ys
                in
                    Just(xs ++ [y] ++ zs)
            else
                Nothing
        _ ->
            Nothing
        

-- | Search for type correct programs
searchIter :: [Instruction] -> [(Stack, Val)] -> StackType -> Program -> Int -> Int -> [Program]
searchIter instructions goals stackType current currentDepth limit  = 
    let
        -- Possible programs given current instruction set
        typeCorrect = catMaybes $ map (\i -> (\ty -> (ty, i:current)) `fmap` typeOf i stackType) instructions
        -- Prune non-productive programs
        notIdentity = filter (\(_, program) -> not (isIdentityOrNotOk program)) typeCorrect
        -- filter on "result" type (singular value)
        isFinal = filter (\(ty, _) -> isResult ty) notIdentity
        --  Find whether there are programs at current depth satisfy the goals TODO: undo reverse
        satisfiesAll = filter (\(_, program) ->
            let p = reverse program in all (\(s, g) -> exec p p s zeroState == g) goals) isFinal
    in
        if currentDepth == limit then
            map snd satisfiesAll
        else
            concatMap (\(ty, program) -> searchIter instructions goals ty program (currentDepth + 1) limit) notIdentity

-- | Find programs given input stacks and result (from shortest to higher, if possible)
-- | This will not terminate if goals are not possible given instruction set and goals
search :: [Instruction] -> [(Stack, Val)] -> [Program]
search instructions goals =
    case goals of
        -- Compute first type, assume for now that all goals are of equal type
        (x, _): _ ->
            -- use iterative deepening
            map reverse . filter (/=[]) $ concatMap (searchIter instructions goals (getTypeOfStack x) [] 1) [1..]
        _ ->
            []


{- Examples

instructionSet = [Sts(-2), Sts(-3), Lds (-2), Lds (-1), Int32Push 1, Int32Add, Int32Mul, Int32Div, Int32PopCount, Int32Inc]

-- Find program that computes f(x) = x ^ 3 + 1
pow3PlusOneGoals = [([Int32Val 2], Int32Val 9), ([Int32Val 3], Int32Val 28), ([Int32Val 4], Int32Val 65)]
pow3PlusOneProgram = head $ search instructionSet pow3PlusOneGoals


-- Find program that computes f(x, y) = popCount(x) + popCount(y)
popCounts2 = [([Int32Val 1, Int32Val 1], Int32Val 2), ([Int32Val 2, Int32Val 3], Int32Val 3), ([Int32Val 7, Int32Val 7], Int32Val 6)]
popCounts2Program = head $ search instructionSet popCounts2

-}
