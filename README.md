# Type Constrained Program Search
Type Constrained Program Search: Program search for type correct programs.


Examples

=== 
```haskell
-- Use this set of instructions
instructionSet = [Lds (-1), Int32Push 1, Int32Add, Int32Mul, Int32Div]

-- Create goals: the program should produce all these values given values on stack
-- We want to find a program that computes f(x) = x^3 + 1
pow3PlusOneGoals = [([Int32Val 2], Int32Val 9), ([Int32Val 3], Int32Val 28), ([Int32Val 4], Int32Val 65)]

-- Find a program that satisfies our constraints
-- Results in this (correct) program:  [Lds (-1),Lds (-1),Int32Mul,Int32Mul,Int32Push 1,Int32Add]
pow3PlusOneProgram = head $ search instructionSet pow3PlusOneGoals
```
