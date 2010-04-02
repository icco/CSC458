Nathaniel "Nat" Welch
CSC 458 - Computational Finance
Lab 1

Assignment:
 
- Write a program that computes the growth of a checking account with no
  deposits, compounded annually at an annual rate of 5% for 50 years. No, don't
  use a mutable variable.

- Using System.Random and NextDouble(), write a function that returns 0 with
  probability 10%, 1 with probability 50%, 2 with probability 5%, and 3 with
  probability 35%.

- Develop the manySamples function, that accepts a number i and computes the
  sum of i choices from the prior distribution.

- generalize this function so that it accepts a distribution (represented as a
  function from unit -> int) and computes the sum of i choices from that
  distribution.

- generalize the earlier distribution function so that it accepts a (sorted)
  array of numbers in the range (0.0, 1.0) and returns random numbers chosen
  from the distribution represented by that array.  So, for instance, the array
  [| 0.3; 0.9 |] would represent a 30% chance of getting a 0 and a 60% chance
  of getting a 1 and a 10% chance of getting a 2.

- Using these functions, tabulate the results of 10,000 trials of summing 100
  choices from this distribution. Sketch the curve.  What shape does the curve
  have?

How to run and compile:

If you run make, the default will run all of the parts of the assignment

