# Type Constrained Program Search
Type Constrained Program Search: Program search for type correct programs.

This repository contains an implementation of what I call Type Constrained Program Search. It searches for all programs from small to big that are both typ correct (resulting in a much smaller search space) and satisfy all program goals.


Examples
=== 
```haskell
-- Use this set of instructions
instructionSet = [Sts(-2), Sts(-3),  Lds (-2), Lds (-1), Int32Push 1, Int32Add, Int32Mul, Int32Div, Int32PopCount, Int32Inc]

-- Create goals: the program should produce all these values given values on stack
-- We want to find a program that computes f(x) = x^3 + 1
pow3PlusOneGoals = [([Int32Val 2], Int32Val 9), ([Int32Val 3], Int32Val 28), ([Int32Val 4], Int32Val 65)]

-- Find a program that satisfies our constraints
-- This results in this (correct) program:  [Lds (-1),Lds (-1),Int32Mul,Int32Mul,Int32Inc]
pow3PlusOneProgram = head $ search instructionSet pow3PlusOneGoals

-- Goals for adding two popCounts: f(x, y) = popCount(x) + popCount(y)
popCounts2 = [([Int32Val 1, Int32Val 1], Int32Val 2), ([Int32Val 2, Int32Val 3], Int32Val 3), ([Int32Val 7, Int32Val 7], Int32Val 6)]

-- Finds as first result: [Lds (-2),Int32PopCount,Sts (-3),Int32PopCount,Int32Add]
popCounts2Program = head $ search instructionSet popCounts2

```
