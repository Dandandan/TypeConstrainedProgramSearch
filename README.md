# Type Constrained Program Search
Type Constrained Program Search: Super optimizing program search for type correct programs with equivalence pruning.

Examples
=== 

```haskell
-- Create goals: the program(s) should produce all input / output pairs
-- We want to find a program that computes f(x) = x^3 + 1
pow3PlusOneGoals = [([Int32Val 2], [Int32Val 9]), ([Int32Val 3], [Int32Val 28]), ([Int32Val 4], [Int32Val 65])]

-- Find a program that satisfies our constraints
-- This results in this (correct) program:  [Lds (-1),Lds (-1),Int32Mul,Int32Mul,Int32Inc]
pow3PlusOneProgram = head $ search defaultInstructionSet pow3PlusOneGoals identity5Tree

-- Goals for adding two popCounts: f(x, y) = popCount(x) + popCount(y)
popCounts2Goals = [([Int32Val x, Int32Val y], [Int32Val (fromIntegral $ popCount x + popCount y)]) | x <- [-100..100], y <- [-100..100]]

-- Finds as first result: [Int32PopCount,Lds (-2),Int32PopCount,Int32Add,Sts (-2)]
popCounts2Program = head $ search defaultInstructionSet popCounts2Goals identity5Tree

```
