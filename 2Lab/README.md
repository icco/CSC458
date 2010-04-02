Nathaniel "Nat" Welch
CSC 458 - Computational Finance
Lab 2

Assignment:
 
Develop the function f1 with type (double -> double) defined by
this equation: f1(x) = 3x^2 + 14

Develop the function differ, that consumes a function and a value for x
 and produces an approximation of the value of the function's first derivative
 at that point, by using the value of f at x and at x + 10^-9.

Develop the function oneStep, that consumes a double and either
 increases it by 1 percent or decreases it by 1 percent, randomly .
 Writing a test case for this is tricky. Do your best.

Develop the function nSteps, that consumes a double and an int >= 0 and returns
the result of applying the oneStep function n times. Again,
give some thought to a reasonable test case.

Run 1,000 trials of this function, with n = 20 and starting value 1.0. What does the distribution
look like?

Run 1,000 trials of this function, with n = 40 and starting value 1.0. What does the distribution look
like? How does it compare to the prior distribution?

How to run and compile:

If you run make, the default will run all of the parts of the assignment

