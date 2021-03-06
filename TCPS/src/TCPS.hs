module TCPS where

import           Data.Bits  (complement, popCount, shiftL, shiftR, xor, (.&.),
                             (.|.))
import           Data.Int   (Int32)
import           Data.List  (sort, isInfixOf)
import           qualified  Data.Map as Map (Map, fromList, (!), lookup, foldWithKey, union, adjust, filterWithKey)
import           Data.Maybe (mapMaybe)

-- | Possible base types
data BaseType = Int32Type | Float32Type deriving (Eq, Show)

-- | Type of current stack
type StackType = [BaseType]

-- | Table for looking up successors
type SuccTable = (Int, Map.Map [Instruction] [Instruction])

-- | Some implemented instruction
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
    | Int32ShiftL -- | Integer32 Shift left
    | Int32ShiftR -- | Integer32 Shift right
    | Int32Eq  -- |Integer32 Equality
    | Int32Lt  -- | Integer32 Less than
    | Int32Gt  -- | Integer32 Greater than
    | Int32Lte -- | Integer32 Less than equal
    | Int32Gte -- | Integer32 Greater than equal
    | Int32Cmp -- | Integer32 Compare (resulting in -1 if smaller, 0 if equal and 1 if bigger)
    | Int32Min -- | Integer32 Minimum
    | Int32Max -- | Integer32 Maximum
    | Float32Push Float -- | Float32 Push constant to stack
    | Float32Add -- | Float32 Addition
    | Float32Mul -- | Float32 Multiplication
    | Float32Sub -- | Float32 Substraction
    | Float32Div -- | Float32 Division
    | Float32Neg -- | Float32 Negation
    | Lds Int -- | Load value relative to stack pointer
    | Sts Int -- | Store value relative to stack pointer
    -- | TODO: make custom instructions / BaseType possible
    deriving (Show, Eq, Ord)

-- | A program is list of instructions
type Program = [Instruction]

-- | Primitive values
data Val = Int32Val Int32 | Float32Val Float | Error deriving (Show, Eq, Ord)

-- | Stack of primitive values
type Stack = [Val]

-- | Executes program given program code, stack and state (emulator)
exec :: Program -> Stack -> [Val]
exec todo stack  =
    case (todo, stack) of
        ([], xs) ->
            xs

        (Int32Push i: xs, ys) ->
            exec xs (Int32Val i: ys)

        (Int32Add: xs, Int32Val y1:Int32Val y2: ys) ->
            exec xs (Int32Val (y1 + y2): ys)

        (Int32Sub: xs, Int32Val y1:Int32Val y2: ys) ->
            exec xs (Int32Val (y1 - y2): ys)

        (Int32Mul: xs, Int32Val y1:Int32Val y2: ys) ->
            exec xs (Int32Val (y1 * y2): ys)

        (Int32Div: xs, Int32Val y1:Int32Val y2: ys) ->
            if y2 /= 0 && not(y2 == (-1) && y1 == minBound) then
                exec xs (Int32Val (y1 `div` y2): ys)
            else
                [Error]

        (Int32Mod: xs, Int32Val y1:Int32Val y2: ys) ->
            if y2 /= 0 then
                exec xs (Int32Val (y1 `mod` y2): ys)
            else
                [Error]

        (Int32Neg: xs, Int32Val y1: ys) ->
            exec xs (Int32Val (-y1): ys)

        (Int32Inc: xs, Int32Val y: ys) ->
            exec xs (Int32Val (y + 1): ys)

        (Int32Dec: xs, Int32Val y: ys) ->
            exec xs (Int32Val (y - 1): ys)

        (Int32And: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (y1 .&. y2): ys)

        (Int32Or: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (y1 .|. y2): ys)

        (Int32Xor: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (y1 `xor` y2): ys)

        (Int32PopCount: xs, Int32Val y: ys) ->
            exec xs (Int32Val (fromIntegral $ popCount y): ys)

        (Int32Not: xs, Int32Val y: ys) ->
            exec xs (Int32Val (complement y): ys)

        (Int32ShiftL: xs, Int32Val y: Int32Val shift: ys) ->
            if shift >= 0 then
                exec xs (Int32Val (shiftL y (fromIntegral shift)): ys)
            else
                [Error]

        (Int32ShiftR: xs, Int32Val y: Int32Val shift: ys) ->
            if shift >= 0 then
                exec xs (Int32Val (shiftR y (fromIntegral shift)): ys)
            else
                [Error]

        (Int32Lt: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (if y1 < y2 then 1 else 0): ys)

        (Int32Gt: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (if y1 > y2 then 1 else 0): ys)

        (Int32Lte: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (if y1 <= y2 then 1 else 0): ys)

        (Int32Gte: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (if y1 >= y2 then 1 else 0): ys)

        (Int32Cmp: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (if y1 < y2 then (-1) else (if y1 == y2 then 0 else 1)): ys)

        (Int32Min: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (min y1 y2): ys)

        (Int32Max: xs, Int32Val y1: Int32Val y2: ys) ->
            exec xs (Int32Val (max y1 y2): ys)

        (Float32Push i: xs, ys) ->
            exec xs (Float32Val i: ys)

        (Float32Add: xs, Float32Val y1: Float32Val y2: ys) ->
            exec xs (Float32Val (y1 + y2): ys)

        (Float32Mul: xs, Float32Val y1: Float32Val y2: ys) ->
            exec xs (Float32Val (y1 * y2): ys)

        (Float32Sub: xs, Float32Val y1: Float32Val y2: ys) ->
            exec xs (Float32Val (y1 - y2): ys)

        (Float32Div: xs, Float32Val y1: Float32Val y2: ys) ->
            exec xs (Float32Val (y1 / y2): ys)

        (Float32Neg: xs, Float32Val y1: ys) ->
            exec xs (Float32Val (-y1) : ys)

        (Lds i: xs, ys) ->
            exec xs (ys !! (abs i - 1): ys)

        (Sts i: xs, y:ys) ->
            let
                (z,_:zs) = splitAt (abs i - 2) ys
                new = z ++ [y] ++ zs
            in
                exec xs new
        _ ->
            error "exec: this should not have happened"

-- | Gets StackType given values on stack
typeOfStack :: Stack -> StackType
typeOfStack (Int32Val _: xs) = Int32Type: typeOfStack xs
typeOfStack (Float32Val _: xs) = Float32Type: typeOfStack xs
typeOfStack _ = []

-- | Computes the type of the program when adding the
-- |  instruction if it's type correct.
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
        (Int32ShiftL, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32ShiftR, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32PopCount, Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Lt, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Gt, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Lte, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Gte, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Cmp, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Min, Int32Type: Int32Type: ys) ->
            Just(Int32Type: ys)
        (Int32Max, Int32Type: Int32Type: ys) ->
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

-- | Lookup successors given program context
lookupSuccs :: [Instruction] -> SuccTable -> [Instruction]
lookupSuccs instructions (_, t) = 
    case Map.lookup instructions t of
        Nothing ->
            []
        Just nextInstructions ->
            nextInstructions

-- | Search for type correct programs that satisfy the goals given some instructions
searchIter :: [Instruction] -> [(Stack, [Val])] -> StackType -> StackType -> Program -> SuccTable -> Int -> Int -> [Program]
searchIter instructions goals inputStackType outputStackType current succTable@(succDepth, _) currentDepth limit  =
    let
        successors = lookupSuccs (reverse (take (succDepth - 1) current)) succTable
        -- Use only possible programs given current instruction set
        typeCorrect = mapMaybe (\i -> (\ty -> (ty, i:current)) `fmap` typeOf i inputStackType) successors
        -- Don't run test when current type != output type
        isOutput = filter (\(ty, _) -> ty == outputStackType) typeCorrect
        --  Find whether there are programs at current depth satisfy the goals TODO: undo reverse
        satisfiesAll = filter (\(_, program) ->
            let stackProg = reverse program in all (\(s, g) -> exec stackProg s == g) goals) isOutput
    in
        if currentDepth == limit then
            map snd satisfiesAll
        else
            concatMap (\(ty, program) -> searchIter instructions goals ty outputStackType program succTable (currentDepth + 1) limit) typeCorrect

-- | Some instruction set to test on
defaultInstructionSet :: [Instruction]
defaultInstructionSet = [
    Int32Max, Int32Min,Int32And, Int32Or, Int32Xor, Int32Cmp, Sts(-2), Sts(-3), Sts(-4), Int32Neg,
    Lds (-4), Lds (-3), Lds (-2), Lds (-1), Int32Push (-1), Int32Push 0, Int32Push 1,
    Int32Add, Int32Sub, Int32Mul, Int32Div, Int32PopCount, Int32Inc, Int32Dec, Int32ShiftL, Int32ShiftR]

-- | Find programs that are equivalent to smaller programs
equivalentPrograms :: Int -> SuccTable -> [Program]
equivalentPrograms maxSize table =
    let
        progs = [[], [Int32Dec], [Int32Inc], [Int32PopCount], [Int32Push 1], [Int32Push 0], [Int32Push (-1)], [Lds (-1)], [Int32Neg]]
        goals = map (\p -> [([Int32Val x], exec p [Int32Val x]) | x <- [-100..100]]) progs

        progs2 = [[Int32Add], [Int32Sub], [Int32Cmp], [Int32Max], [Int32Min], [Int32And],
                  [Int32Or], [Int32Xor], [Int32Mul], [Int32Div], [Int32Push 1], [Int32Push 0],
                  [Int32Push (-1)], [Sts (-1)], [Lds (-2)], [Lds (-2), Lds (-1)], [Lds (-1), Lds (-2)], [Lds (-1), Lds (-1)], [Lds (-1), Lds (-3)],
                  [Lds (-2), Lds (-3)]
                ] ++ progs
        goals2 = map (\p -> [([Int32Val x, Int32Val y], exec p [Int32Val x, Int32Val y]) | x <- [-10..10], y <-[-10..10]]) progs2

        smallerThan = takeWhile (\x -> length x <= maxSize)

        allProgsAndGoals = zip progs goals ++ zip progs2 goals2
    in
        concatMap (\(prog, progGoals) -> filter (/= prog) $ smallerThan (search defaultInstructionSet progGoals table)) allProgsAndGoals

-- | List of programs (probably) equivalent to f(x) = x
equivalentPruned :: Int -> SuccTable -> SuccTable
equivalentPruned limit succTable@(depth, _) =
    let
        -- iteratively prune successor table
        smaller = equivalentPrograms depth succTable
        prunedTable = pruneSuccTable smaller succTable
    in
        if depth < limit then
            equivalentPruned limit (expandSuccTable prunedTable)
        else
            prunedTable

-- | Prune the succession table from the top
-- | For example: Decrement followed by Increment can be pruned
pruneSuccTable :: [Program] -> SuccTable -> SuccTable
pruneSuccTable pruneList (depth, table) =
    -- remove each succesor from the table given a program (prefix + successor)
    (depth, foldr (\program t -> let i = last program in Map.adjust (\x -> filter (/= i) x) (init program) t) table pruneList)

-- | Expand succession table
-- | and/or pruned agressively
expandSuccTable :: SuccTable -> SuccTable
expandSuccTable (depth, currTable) =
    (depth + 1, Map.foldWithKey (\k next t -> Map.union t (Map.fromList [(k ++ [n], currTable Map.! (tail (k ++ [n]))) | n <- next])) currTable currTable)

-- | Find programs given input stacks and result (from shortest to higher, if possible)
-- | This will not terminate if goals are not possible given instruction set and goals
search :: [Instruction] -> [(Stack, [Val])] -> SuccTable -> [Program]
search instructions goals table =
    case goals of
        -- Compute first type, assume for now that all goals are of equal type
        (x, y): _ ->
            -- use iterative deepening
            map reverse . filter (/=[]) $ concatMap (searchIter instructions goals (typeOfStack x) (typeOfStack y) [] table 0) [0..]
        _ ->
            []

simpleSuccTable :: [Instruction] -> SuccTable
simpleSuccTable instructionSet = 
    (1, Map.fromList [([], instructionSet)])

-- | Stores all possible successors, somewhat pruned
prunedTableN :: Int ->  SuccTable
prunedTableN depth = equivalentPruned depth (simpleSuccTable defaultInstructionSet)

prunedTable4 :: SuccTable
prunedTable4 = prunedTableN 4

prunedTable5 :: SuccTable
prunedTable5 = prunedTableN 5

{- Examples


-- Find program that computes f(x) = x ^ 3 + 1
pow3PlusOneGoals = [([Int32Val 2], [Int32Val 9]), ([Int32Val 3], [Int32Val 28]), ([Int32Val 4], [Int32Val 65])]
pow3PlusOneProgram = head $ search defaultInstructionSet pow3PlusOneGoals prunedTable4

-- Find program that computes f(x, y) = popCount(x) + popCount(y)
popCounts2Goals = [([Int32Val x, Int32Val y], [Int32Val (fromIntegral $ popCount x + popCount y)]) | x <- [-100..100], y <- [-100..100]]
popCounts2Program = head $ search defaultInstructionSet popCounts2Goals prunedTable4

-- leftShift by 3 without instruction Int32Push 3
-- [Int32Push 2,Int32Push 2,Int32ShiftL,Int32Mul]
leftshiftGoals = [([Int32Val i], [Int32Val (shiftL (fromIntegral i) 3)]) | i <- [-100..100]]
leftshiftProgram = head $ search defaultInstructionSet leftshiftGoals prunedTable4


absGoals = [([Int32Val i], [Int32Val (abs i)]) | i <- [-100..100]]
absProgram = head $ search defaultInstructionSet absGoals prunedTable4

minimumGoals = [([Int32Val i, Int32Val j], [minimum [Int32Val i, Int32Val j]]) | i <- [-10..10], j <- [-10..10]]
minimumProgram = head $ search defaultInstructionSet minimumGoals prunedTable4


-- Result: [Lds (-2),Lds (-3),Lds (-3),Int32Max,Sts (-4),Int32Min]
sort2Goals = [([Int32Val i, Int32Val j], sort [Int32Val i, Int32Val j]) | i <- [-10..10], j <- [-10..10]]
sort2Program = head $ search defaultInstructionSet sort2Goals prunedTable4

sort3Goals = [([Int32Val i, Int32Val j, Int32Val k], sort [Int32Val i, Int32Val j, Int32Val k]) | i <- [-10..10], j <- [-10..10], k <- [-10..10]]
sort3Program = head $ search defaultInstructionSet sort3Goals prunedTable4
-}
